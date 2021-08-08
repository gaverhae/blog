{:title "Cheap interpreter, part 8: faster register machines"
 :layout :post
 :tags ["cheap interpreter"]}

[Last week][part 7] I briefly presented the basics of register machines: what
they are and how to write a simple compiler and interpreter for an example
instruction set. In this post, I'll show a few tricks that can be used to make
this example quite a bit faster.

> This [series] is based on [Neil Mitchell][ndm]'s talk "[Cheaply writing a fast
> interpeter][cwafi]". The talk compares a number of approaches to writing an
> interpreter and tries to find a good balance between complexity and interpreter
> overhead.
>
> The following topics, while important, are out of scope:
>
> - Parsing. We're assuming we start with a parse tree.
> - Producing assembly code: the definition of "cheap" that Neil uses in the talk
>   is "maintainers are not required to know another language" (specifically
>   assembly).
> - Semantic optimizations (constant folding, algebraic transformations, etc.).
>   The goal is to compare the interpretation overhead of various approaches, and
>   semantic optimizations are considered orthogonal to that.
> - JIT is not explicitly excluded in the talk, but it is completely absent. This
>   is probably also a consequence of the "cheap" constraint.

## Array-based interpreter

The first thing we'll do should be no surprise if you've read the [faster stack
machines][part 6] part of [this series][series]: we're going to rewrite our
interpreter to use a mutable array instead of an immutable map to keep track of
the contents of our registers. We start by changing the `[RegOp]` into a
`Vector`:

```haskell
run_registers_2 :: [RegOp] -> Int -> Int
run_registers_2 ls_code = do
  let code :: Data.Vector RegOp
      !code = Data.Vector.fromList ls_code
```

Unlike the stack machine, where we took a random guess at the maximum stack
size, here we can know the total number of registers statically:

```haskell
  let max_reg :: Int
      !max_reg = foldl (\acc el -> max acc $ case el of
        RegLoadLiteral (Register to) _ -> to
        RegLoad (Register to) _ -> to
        RegBin _ (Register to) _ _ -> to
        _ -> 0) 0 ls_code
```

Since we were already using essentially a mapping from int to int, going from
the map-based code to an array-based version is not all that interesting, so
here's the loop code in its entirety:

```haskell
  let loop :: forall s. Data.Vector.Unboxed.Mutable.MVector s Int -> Int -> Control.Monad.ST.ST s Int
      loop regs ip = case (Data.Vector.!) code ip of
        RegEnd (Register r) -> read regs r >>= return
        RegLoadLiteral (Register to) val -> do
          write regs to val
          loop regs (ip + 1)
        RegLoad (Register to) (Register from) -> do
          v <- read regs from
          write regs to v
          loop regs (ip + 1)
        RegJumpIfZero (Register r) to -> do
          v <- read regs r
          if 0 == v
          then loop regs to
          else loop regs (ip + 1)
        RegJump to -> loop regs to
        RegBin op (Register to) (Register a1) (Register a2) -> do
          v1 <- read regs a1
          v2 <- read regs a2
          write regs to (bin op v1 v2)
          loop regs (ip + 1)
        where
        write = Data.Vector.Unboxed.Mutable.write
        read = Data.Vector.Unboxed.Mutable.read
```

and, finally, we can return a function that creates the array of length
`max_reg` and runs the `loop` function on it:

```haskell
  \_ -> Control.Monad.ST.runST $ do
    registers <- Data.Vector.Unboxed.Mutable.unsafeNew (max_reg + 1)
    loop registers 0
```

I'll refer you to the [stack machine part][part 6] for all the relevant
warnings about introducing mutation. They're still just as valid.

This gets us in the vicinity of our stack machine; specifically, with normal
lazy evaluation:

```plaintext
exec_stack_2 (3000 runs): 244.13 ms (81 µs/run)
run_registers_2 (3000 runs): 378.90 ms (126 µs/run)
```

and with `Strict` enabled:

```plaintext
exec_stack_2 (3000 runs): 224.58 ms (74 µs/run)
run_registers_2 (3000 runs): 257.63 ms (85 µs/run)
```

We can do a bit better, though.

## Setting variables

At this point the interpreter is optimized to the best of my Haskell knowledge
(which, granted, doesn't mean all that much). I can imagine there may be a few
tweaks that get our "normal" Haskell closer to the "strict" runtime, but I
can't think of an approach that would make the interpreter massively more
performant.

There is something else we can improve, however. Let's take another look at the
generated register code:

```haskell
[ RegLoadLiteral (Register 2) 100
, RegLoad (Register 0) (Register 2)
, RegLoadLiteral (Register 3) 1000
, RegLoad (Register 1) (Register 3)
, RegLoadLiteral (Register 4) 0
, RegBin NotEq (Register 5) (Register 4) (Register 1)
, RegJumpIfZero (Register 5) 22
, RegLoadLiteral (Register 6) 4
, RegBin Add (Register 7) (Register 0) (Register 6)
, RegBin Add (Register 8) (Register 7) (Register 0)
, RegLoadLiteral (Register 9) 3
, RegBin Add (Register 10) (Register 8) (Register 9)
, RegLoad (Register 0) (Register 10)
, RegLoadLiteral (Register 11) 2
, RegBin Add (Register 12) (Register 0) (Register 11)
, RegLoadLiteral (Register 13) 4
, RegBin Add (Register 14) (Register 12) (Register 13)
, RegLoad (Register 0) (Register 14)
, RegLoadLiteral (Register 15) (-1)
, RegBin Add (Register 16) (Register 15) (Register 1)
, RegLoad (Register 1) (Register 16)
, RegJump 4
, RegEnd (Register 0)
]
```

There is a pattern that seems a bit wasteful there. Here's one of its instance:

```haskell
, RegBin Add (Register 10) (Register 8) (Register 9)
, RegLoad (Register 0) (Register 10)
```

These are the only two appearances of register 10. As a human looking at this,
it seems pretty obvious that we could just do

```haskell
, RegBin Add (Register 0) (Register 8) (Register 9)
```

directly instead. This pattern appears three times (registers 10, 14 and 16) in
a 15-instructions loop which represents the majority of our execution time.
Cutting it would mean going from an inner loop of 15 instructions to 12, so we
can expect a significant (~20%) performance improvement if we were able to
apply that optimization.

We could add a pass over the generated code, but in this case it is simple
enough to change the compiler itself to generate this directly. Indeed, this
pattern is a direct result of the `Set r exp` instruction: whatever `exp` is,
with our current compiler it will always end up writing its result to a new
register, and then `Set` will add a copy from that register to the variable.

To implement this, we can change the `eval` function in our compiler to take in
an additional `Maybe Register` argument. Most recursive calls must be changed
to set it explicitly to `Nothing`, but the one in the `Set` case is changed to
pass along the variable it's supposed to set:

```haskell
    Set idx exp1 -> do
      Just (Register r) <- eval (Just (Register idx)) exp1
      when (r /= idx) (RegEmit (RegLoad (Register idx) (Register r)))
      return Nothing
```

For reference, the previous version of this case had this body:

```haskell
      Just r <- eval exp1
      RegEmit (RegLoad (Register idx) r)
      return Nothing
```

So now we:

- Pass along the current register to the recursive `eval` call, and
- Check if it has indeed set the expected register, and if so, skip emitting
  the `RegLoad` instruction.

In a perfect world that second step might not be needed, but we're being a bit
defensive here. Extra steps at compile time are not a performance issue, and
I'd rather have working code than optimized-but-wrong code.

Technically, this situation could be considered as a bug in the optimizer: some
instruction has received a `Just r` for its return value and ignored it. From
that perspective, having the second step throw an exception instead of emitting
a correcting `RegLoad` may also be a valid choice here. I'd encourage the
reader to think through what should happen for the `Set 0 (Var 1)` expression,
though.

The other change we need to do is change the relevant instructions to actually
write to the supplied register instead of generating a new one. For our code
sample, the only two instructions that are used as the top-level expression in
a `Set` are `Bin` and `Lit`. The change is conceptually identical in both of
them; replace:

```haskell
      r <- RegNext
```

with

```haskell
      r <- case ret of
        Nothing -> RegNext
        Just r -> return r
```

Finally, the recursive `eval` call on `rest` in the `Do` case should thread
through the intended return register:

```
    Do first rest -> do
      _ <- eval Nothing first
      r <- eval ret rest
      return r
```

though that case does not appear in our sample code.

With those changes, the generated code becomes:

```haskell
[ RegLoadLiteral (Register 0) 100
, RegLoadLiteral (Register 1) 1000
, RegLoadLiteral (Register 2) 0
, RegBin NotEq (Register 3) (Register 2) (Register 1)
, RegJumpIfZero (Register 3) 17
, RegLoadLiteral (Register 4) 4
, RegBin Add (Register 5) (Register 0) (Register 4)
, RegBin Add (Register 6) (Register 5) (Register 0)
, RegLoadLiteral (Register 7) 3
, RegBin Add (Register 0) (Register 6) (Register 7)
, RegLoadLiteral (Register 8) 2
, RegBin Add (Register 9) (Register 0) (Register 8)
, RegLoadLiteral (Register 10) 4
, RegBin Add (Register 0) (Register 9) (Register 10)
, RegLoadLiteral (Register 11) (-1)
, RegBin Add (Register 1) (Register 11) (Register 1)
, RegJump 2
, RegEnd (Register 0)
]
```

We've gained 5 instructions, though only 3 of them within the loop. The runtime
improves, as expected, by about 20%:

```plaintext
run_registers_2 (3000 runs): 315.89 ms (105 µs/run)
```

or a little bit less with `Strict`:

```plaintext
run_registers_2 (3000 runs): 236.87 ms (78 µs/run)
```

## Constants should not change

Looking at that code, there's still quite a bit of waste, though. By
definition, literals don't change, so it's a bit of a shame that we have so
many `RegLoadLiteral` instructions in our tight loop.

Again, we could conceive of a second pass of optimization moving those
instructions out of the loop, but we will instead change our compiler to take
care of it directly.

First off, this requires a way to keep track of those constants as we go
through the compilation process. To do that, we'll expand our monadic state
with a new `Env` field:

```haskell
data RegState = RegState { num_registers :: Int
                         , code :: [RegOp]
                         , hoisted :: Env -- this is new
                         }
```

and our set of monadic actions with one to add a constant to that mapping:

```haskell
data RegExec a where
  RegBind :: RegExec a -> (a -> RegExec b) -> RegExec b
  RegReturn :: a -> RegExec a
  RegEmit :: RegOp -> RegExec ()
  RegNext :: RegExec Register
  RegPosition :: RegExec Int
  RegEmitBefore :: (Int -> RegOp) -> RegExec () -> RegExec ()
  RegHoist :: Int -> RegExec Register -- this is new
```

Next, we need to change the single case `Lit` in the `eval` function of our
compiler to hoist the literal if it is not currently being set on a variable:

```haskell
    Lit v -> do
      case ret of
        Nothing -> RegHoist v >>= return . Just
        Just r -> do
          RegEmit (RegLoadLiteral r v)
          return (Just r)
```

That's all for the compiler (+ initializing `hoisted` to `mt_env`), but we now
need to change our interpreter to know about this new field. Moreover, we need
to change the type passed from the compiler to the interpreter to contain that
field. Whereas before we passed only the resulting `[RegOp]`, we're now going
to pass along the full `RegState`.

This is not the only possible choice: our compiler could also just emit all of
the `RegLoadLiteral` instructions at the beginning of the `[RegOp]`, and our
interpreter would then not need any change. The tradoff there is that requires
changing the jump instructions.

Besides the trivial unwrapping of `RegState`, there are two semantic changes
required to the interpreter code to accommodate this. The first one is to
change the computation of `max_reg` to include the literals:

```haskell
  let max_reg :: Int
      !max_reg =
        max (foldl (\acc el -> max acc $ case el of
               RegLoadLiteral (Register to) _ -> to
               RegLoad (Register to) _ -> to
               RegBin _ (Register to) _ _ -> to
               _ -> 0) 0 (code rs))
            (reduce (\acc (r, _) -> max acc r) 0 (hoisted rs))
```

where I have defined `reduce` as:

```haskell
reduce :: (b -> (Int, Int) -> b) -> b -> Env -> b
reduce f zero (Env m) = foldl f zero (Data.Map.toList m)
```

The second one is to intialize the constants once, before running the code:

```haskell
  \_ -> Control.Monad.ST.runST $ do
    registers <- Data.Vector.Unboxed.Mutable.unsafeNew (max_reg + 1)
    forM_ (reduce (\acc el -> el:acc) [] $ hoisted rs) (\(r, v) -> do
      Data.Vector.Unboxed.Mutable.write registers r v)
    loop registers 0
```

The code now looks like:

```haskell
[ RegLoadLiteral (Register 0) 100
, RegLoadLiteral (Register 1) 1000
, RegBin NotEq (Register 3) (Register 2) (Register 1)
, RegJumpIfZero (Register 3) 11
, RegBin Add (Register 5) (Register 0) (Register 4)
, RegBin Add (Register 6) (Register 5) (Register 0)
, RegBin Add (Register 0) (Register 6) (Register 7)
, RegBin Add (Register 9) (Register 0) (Register 8)
, RegBin Add (Register 0) (Register 9) (Register 10)
, RegBin Add (Register 1) (Register 11) (Register 1)
, RegJump 2
, RegEnd (Register 0)
]
```

and we get another nice speed boost:

```plaintext
run_registers_2 (3000 runs): 207.63 ms (69 µs/run)
```

or, with `Strict`:

```plaintext
run_registers_2 (3000 runs): 148.69 ms (49 µs/run)
```

## Going further

We're still over a hundred times slower than the baseline, which clocks in at
about 0.3 microseconds (regardless of `Strict`), so in principle it should be
possible to go further. The Haskell optimizer is not, however, bound by our
self-imposed restrictions. Given that my machine is clocked at 2.8GHz and the
code is supposed to loop a thousand times, 300 nanoseconds means we're actually
running _less_ than one instuction per iteration on average, despite the code
seemingly having 1 comparison, 5 additions and 1 subtraction in each loop, not
counting the `loop` function call itself. Based on those numbers, there is no
way it's not doing at least some level of semantic optimization.

One of the things we could still do to improve speed is to change our bytecode.
If we added a "add literal to register" instruction, we could save the time it
takes us to fetch constants from the registers vector.

Another thing we could do, possibly combined with the above, is semantic
optimizations. Arithmetic analysis could replace the entire loop with `x <- 2x
+ 13`, so we could replace:

```Haskell
, RegBin Add (Register 5) (Register 0) (Register 4)
, RegBin Add (Register 6) (Register 5) (Register 0)
, RegBin Add (Register 0) (Register 6) (Register 7)
, RegBin Add (Register 9) (Register 0) (Register 8)
, RegBin Add (Register 0) (Register 9) (Register 10)
```

with

```haskell
, RegBinRR Add (Register 0) (Register 0) (Register 0)
, RegBinRL Add (Register 0) (Register 0) 4
```

where the suffix `R` means register and `L` means literal (say we wanted to
support all three combinations).

Of course, if we're going to do semantic analysis, given that our sample code
has no free variable we could also just do all of the computation at compile
time, and beat the baseline by just returning `-13` directly. Running the final
`run_registers_2` over this `RegState`:

```haskell
RegState { hoisted = mt_env,
           num_registers = 0,
           code = [RegLoadLiteral (Register 0) (-13),
                   RegEnd (Register 0)]}
```

yields a runtime of 30ns using the same benchmark I've used so far.

## What's next?

This series has been all about writing interpreters, and so far we've been
doing so from a statically compiled language. In the next part of this series,
I'll show a possible advantage of writing interpreters in a dynamic language
instead, where the host language interpreter is available at runtime.

[series]: /tags/cheap%20interpreter
[part 1]: /posts/2021-06-19-cwafi-1
[part 2]: /posts/2021-06-27-cwafi-2
[part 3]: /posts/2021-07-04-cwafi-3
[part 4]: /posts/2021-07-11-cwafi-4
[part 5]: /posts/2021-07-18-cwafi-5-mea-culpa
[part 6]: /posts/2021-07-25-cwafi-6-faster-stack-machine
[part 7]: /posts/2021-07-25-cwafi-7-register-machine
[ndm]: https://ndmitchell.com
[cwafi]: https://www.youtube.com/watch?v=V8dnIw3amLA
