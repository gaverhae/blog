{:title "Cheap interpreter, part 7: register machines"
 :layout :post
 :tags ["cheap interpreter"]}

> **[EDIT 2021-08-15]**: The original version of `RegEmitBefore` did not work
> for nested loops. Huge thanks to Reddit user [JeffJeffJeffersonson] for
> spotting and reporting the issue!

[Last week][part 6] I presented a few ways to improve the performance of the
[very simple stack machine][part 4] we had previously defined. In this post, we
move further into the list of techniques presentetd in [Neil][ndm]'s
[talk][cwafi] and take a look at another type of low-level machine called a
register machine.


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

### Overview

As the name suggests, a register machine is a bit like a stack machine, except
for the fact that state is stored in a set of registers instead of a stack.
Registers have a name (usually a number) and can be written to and read from
individually.

The register machine language will reflect that, with operations to store
things in specific registers and to load things from specific registers. Just
like for a stack machine language, there are many ways to define the set of
operations in a register machine language; for the purpose of this blog post,
we're going to go with the following:

```haskell
newtype Register = Register Int
 deriving Show

data RegOp
 = RegEnd Register
 | RegLoadLiteral Register Int
 | RegLoad Register Register
 | RegJumpIfZero Register Int
 | RegJump Int
 | RegBin Op Register Register Register
 | RegPlaceholder
 deriving Show
```

where:

- `RegEnd r` indicates the end of the program, returning the value currently
  held in register `r`.
- `RegLoadLiteral r i` write literals `i` into register `r`.
- `RegLoad r1 r2` reads the value in register `r2` and writes it into register
  `r1`.
- `RegJumpIfZero r i` reads the value of register `r` and, if it is zero, sets
  the instruction pointer to `i`.
- `RegJump i` sets the instruction pointer to `i`.
- `RegBin op to arg1 arg2` reads the values in registers `arg1` and `arg2`,
  applies the function `op` to them, and then writes the result in register
  `to`.
- `RegPlaceholder` is a placeholder used during compilation. I has no semantics
  and should never appear in any code emitted by the compiler.

As for the stack machine language, by default every instruction also increments
the instruction pointer by one, unless it sets it to a specific value. A
program for our register machine will be a list of such instructions, i.e.
`[RegOp]`.

### Number of registers

At first glance, it may look like we have to decide on a number of registers
before we can move forward from here. This turns out not to be the case: we can
simply assume we have an infinite number of registers. Not only will that be
good enough for our simple sample code here, it's also a [real technique used in
real compilers][ssa].[^ssa]

[^ssa]: To clarify, the code produced in this blog post is *not* in SSA form;
SSA is simply used as a justification for assuming we can have as many
registers as we need.

It works because we're only coming up with new registers as we write new,
static instructions. Therefore, the total number of registers is still
statically known, and a further compilation pass can swap them in and out of
memory as required to fit in the actual number of registers the target machine
has.

Just like we stored our variables on the stack in the [stack machine][part 4],
we'll use the "bottom" registers for our variables here.

### Compilation monad

Whereas the compilation process for the [stack machine][part 4] only needed to
keep track of the size of the generated code, the register machine compilation
process will need to keep track of:

- The size of the generated code so far, for jumps.
- The number of registers assigned so far, to be able to generate new ones.
- The generated code, to be able to return it.
- For each subexpression in a compound expression, the register to which the
  result is written.

It's not _a lot_ more state than the stack machine, but it's enough in my view
to justify tracking it with a monad.

Here is the monad I've come up with:

```haskell
instance Functor RegExec where fmap = liftM
instance Applicative RegExec where pure = return; (<*>) = ap
instance Monad RegExec where return = RegReturn; (>>=) = RegBind
instance MonadFail RegExec where fail = error "Should not happen"

data RegExec a where
  RegBind :: RegExec a -> (a -> RegExec b) -> RegExec b
  RegReturn :: a -> RegExec a
  RegEmit :: RegOp -> RegExec ()
  RegNext :: RegExec Register
  RegPosition :: RegExec Int
  RegEmitBefore :: (Int -> RegOp) -> RegExec () -> RegExec ()
```

with the following semantics:

- `RegEmit op` adds a line of code to the code we're currently building.
- `RegNext` provides a new register.
- `RegPosition` gives the current position, so we can jump to it.
- `RegEmitBefore f m` will first execute `m` to generate some amount of code,
  then run `f` with the position at the end of that code as an argument, and
  insert the result of `f` _before_ the result of executing `m`. This
  implements the "jump over a piece of code" of conditionals (`if`, `while`,
  etc.).
- For instructions that write a result, `a` will be the register where that
  result is written.

or, more precisely, using the `eval`/`exec` split I introduced in [my monad
tutorial][monads] (specifically [part 3][monads-3]):

```haskell
data RegState = RegState { num_registers :: Int
                         , code :: [RegOp]
                         }
 deriving Show

compile_registers :: Exp -> [RegOp]
compile_registers exp =
-- [...]
  where
-- [...]
  exec :: RegExec a -> RegState -> (a -> RegState -> RegState) -> RegState
  exec m cur k = case m of
    RegBind ma f -> exec ma cur (\a cur -> exec (f a) cur k)
    RegReturn a -> k a cur
    RegEmit op ->
      k () (cur { code = (code cur) <> [op] })
    RegNext ->
      k (Register $ num_registers cur) cur { num_registers = (num_registers cur) + 1 }
    RegPosition ->
      k (length (code cur)) cur
    RegEmitBefore f m ->
      let nested = exec m (cur { code = (code cur) <> [RegPlaceholder]}) (\() r -> r)
          cur_len = length (code cur)
      in k () (nested { code = (code cur)
                            <> [f (length (code nested))]
                            <> (drop (cur_len + 1) (code nested)) })
```

From there, the `eval` code is fairly straigthtforward for most cases. Literals
are just written to a new register using `RegLoadLiteral`:

```haskell
compile_registers :: Exp -> [RegOp]
compile_registers exp =
-- [...]
  where
-- [...]
  eval :: Maybe Register -> Exp -> RegExec (Maybe Register)
  eval ret = \case
    Lit v -> do
      r <- RegNext
      RegEmit (RegLoadLiteral r v)
      return (Just r)
-- [...]
```

Variables are mapped directly to their index as a register:

```haskell
    Var idx -> return $ Just $ Register idx
    Set idx exp1 -> do
      Just r <- eval exp1
      RegEmit (RegLoad (Register idx) r)
      return Nothing
```

and `Bin` and `Do` are the usual sequencing:

```haskell
    Bin op exp1 exp2 -> do
      Just r1 <- eval exp1
      Just r2 <- eval exp2
      r <- RegNext
      RegEmit (RegBin op r r1 r2)
      return $ Just r
    Do first rest -> do
      _ <- eval first
      r <- eval rest
      return r
```

The `While` case is a little bit more complicated, as we need to know the
length of the body in order to emit the jump over it. We use the
`RegEmitBefore` operation to get that effect:

```haskell
    While cond body -> do
      before_condition <- RegPosition
      Just condition_result <- eval cond
      RegEmitBefore (\after_body -> RegJumpIfZero condition_result after_body)
                    (do
          _ <- eval body
          _ <- RegEmit (RegJump before_condition)
          return ())
      return Nothing
```

For the initial state of our monadic evaluation, we need to know the total
number of variables (so we can start generating new registers "after" them),
which is easily computed:

```haskell
compile_registers :: Exp -> [RegOp]
compile_registers exp =
  code $ exec (eval exp)
              (RegState { num_registers = (max_var exp) + 1
                          , code = [] })
              (\(Just r) s -> s { code = (code s) <> [RegEnd r]})
  where
  max_var :: Exp -> Int
  max_var = \case
    Lit _ -> 0
    Var idx -> idx
    Set idx exp1 -> max idx (max_var exp1)
    Bin _ exp1 exp2 -> max (max_var exp1) (max_var exp2)
    Do first rest ->  max (max_var first) (max_var rest)
    While cond body -> max (max_var cond) (max_var body)
  eval :: Maybe Register -> Exp -> RegExec (Maybe Register)
-- [...]
  exec :: RegExec a -> RegState -> (a -> RegState -> RegState) -> RegState
-- [...]
```

And we add a `RegEnd` instruction to signal the end of the program. With all of
that, the compiler produces this code:

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

### Simple interpreter

It's easy to write a simple, inefficient interpreter for a register language,
by emulating the registers with a map (I've reused [my `Env` type][part 2] for
that):

```haskell
run_registers :: [RegOp] -> Int -> Int
run_registers code =
  \n -> loop 0 (insert mt_env (-1) n)
  where
  loop :: Int -> Env -> Int
  loop ip regs = case code !! ip of
    RegEnd (Register r) -> lookup regs r
    RegLoadLiteral (Register r) v -> loop (ip + 1) (insert regs r v)
    RegLoad (Register to) (Register from) -> loop (ip + 1) (insert regs to (lookup regs from))
    RegJumpIfZero (Register r) to ->
      loop (if 0 == lookup regs r then to else (ip + 1)) regs
    RegJump to -> loop to regs
    RegBin op (Register to) (Register a1) (Register a2) ->
      loop (ip + 1) (insert regs to (bin op (lookup regs a1) (lookup regs a2)))
    RegPlaceholder -> error "Invalid code"
```

As one would expect, this is very slow. In fact, it's the slowest interpreter
we've seen so far, even slower than the slow stack interpreter.

### Next steps

In the next post, I'll show a few things one can do to make this faster. If
you've read the [faster stack machines][part 6] post in this series, you
probably have some idea of what to expect, but there will be a few twists.

[series]: /tags/cheap%20interpreter
[part 1]: /posts/2021-06-19-cwafi-1
[part 2]: /posts/2021-06-27-cwafi-2
[part 3]: /posts/2021-07-04-cwafi-3
[part 4]: /posts/2021-07-11-cwafi-4
[part 5]: /posts/2021-07-18-cwafi-5-mea-culpa
[part 6]: /posts/2021-07-25-cwafi-6-faster-stack-machine
[ndm]: https://ndmitchell.com
[cwafi]: https://www.youtube.com/watch?v=V8dnIw3amLA
[ssa]: https://en.wikipedia.org/wiki/Static_single_assignment_form
[JeffJeffJeffersonson]: https://www.reddit.com/user/JeffJeffJeffersonson
[monads]: /tags/monad-tutorial
[monads-3]: /posts/2021-05-02-monads-3
