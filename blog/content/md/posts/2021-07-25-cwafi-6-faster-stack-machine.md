{:title "Cheap interpreter, part 6: faster stack machines"
 :layout :post
 :tags ["cheap interpreter"]}

[Last week][part 5] was all about me screwing up my benchmarks. There have
been some very interesting [discussions on reddit][reddit], including
suggestions on how to do better benchmarking in Haskell, and how to better
implement the approaches presented in the previous parts of this series. I
highly recommend reading them.

[Two weeks ago][part 4], before I so rudely interrupted myself with that
diversion on benchmarking, I introduced the basics of what a stack machine is,
with a pretty underwhelming performance result. It may have seemed overly
complicated compared to the other approaches we've seen so far; hopefully this
post, where I show how to make that stack machine interpreter a bit faster,
justifies the inclusion of stack machines in this series.

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

## Faster interpreter

The important point about having a stack machine, when it comes to performance,
is that we're shrinking a possibly fairly large surface language to a
comparably small set of stack machine operations. This means that, when writing
our stack machine interpreter, we can afford to invest a bit more effort into
the implementation.

The easiest way to improve the performance of the interpreter we defined in
[part 4] is to pay attention to the data structures we use. We represented our
stack as a list, which sort of works, but it can be a bit slow. Adding and
removing at the start of a list is reasonably fast, but the nature of lists can
make it hard to achieve memory locality, lists are boxed, and, more
importantly, we're not only working with the top of the stack, but also with
the other end, where we set variables.

The need to access specific indices suggests using an array (or vector) with
direct indexed access. The need to change elements when we set variables
suggests the use of a _mutable_ vector.

Mutation should not be introduced lightly. However, in this case it may be
worth it. We can work with a mutable (unboxed) vector of `Int`s to represent
our stack in the following way:

- We reserve the first N elements of the vector for our variables; walking
  through the stack machine code to find out how many variables are involved is
  easy enough. As [previously discussed][part 5], a correct stack machine
  executing valid code should not "pop back past" its starting point, so starting
  with an offset is not an issue for the rest of the interpreter.
- We keep track of the current position of the top of the stack in the vector.
  Conceptually, we're recording the number of elements we have written so far,
  though we're only manipulating this by incrementing and decrementing it and
  we're not starting at 0.

Working with mutable state in Haskell requires a few incantations to ensure the
mutable state is properly contained. Here is the full code:

```haskell
exec_stack_2 :: [StackOp] -> Int -> Int
exec_stack_2 ls_code =
  \_ -> Control.Monad.ST.runST $ do
    init_stack <- Data.Vector.Unboxed.Mutable.unsafeNew 256
    go init_stack
  where
  num_vars = foldl max 0 ((flip map) ls_code (\case StackGet n -> n; StackSet n -> n; _ -> 0))
  code :: Data.Vector StackOp
  !code = Data.Vector.fromList ls_code
  go :: forall s. Data.Vector.Unboxed.Mutable.MVector s Int -> Control.Monad.ST.ST s Int
  go stack = do
    loop 0 (num_vars + 1)
    where
    loop :: Int -> Int -> Control.Monad.ST.ST s Int
    loop ip top = case (Data.Vector.!) code ip of
      StackPush v -> do
        write top v
        loop (ip + 1) (top + 1)
      StackSet n -> do
        v <- read (top - 1)
        write n v
        loop (ip + 1) (top - 1)
      StackGet n -> do
        v <- read n
        write top v
        loop (ip + 1) (top + 1)
      StackBin op -> do
        a2 <- read (top - 1)
        a1 <- read (top - 2)
        write (top - 2) (bin op a2 a1)
        loop (ip + 1) (top - 1)
      StackJump i -> loop i top
      StackJumpIfZero i -> do
        v <- read (top - 1)
        if v == 0
        then loop i (top - 1)
        else loop (ip + 1) (top - 1)
      StackEnd -> do
        v <- read (top - 1)
        return v
    write = Data.Vector.Unboxed.Mutable.write stack
    read = Data.Vector.Unboxed.Mutable.read stack
```

This is not beautiful code. There's mutation, of course, but also a lot of
places for off-by-one errors to hide. But this is also not a lot of code, so
there may be projects in which this approach can be justified.

A few salient points to note:

- We can start with "unsafe" (i.e. uninitialized) memory because we trust the
  stack code we're going to interpret. This works for an interpreter, where the
  `compile_stack` function is run as part of the same process. If the stack code
  were to be somehow stored and loaded back, this could become an issue.
- I've arbitrarily set the size of the stack to 256 because that's enough to
  run this example. In a real implementation, a bigger size may be needed. Note
  that we are using bound-checking `read` and `write` operations, so there is
  little risk of memory exposure through this, even if we were loading untrusted
  stack machine code.
- One might wonder why the stack size is set at all: we could have made it a
  self-adjusting vector. That would be slower, of course, but also much more
  flexible. It would also be more complex and out of scope for this blog
  series. Note that having a fixed stack is not all that weird either: many
  "real" VMs have a set-at-startup stack size too.
- The function argument is not used; unlike our [previous approaches][part 5],
  this one does not seem to get inlined or cached by the compiler. I suspect
  the use of mutable state is responsible for that.
- Since we're going to jump around in the code, and we've already added the
  [vector] package, we also convert our stack machine code to a vector before
  running. We want that to happen once and be cached, and sort of hope that the
  `!` achieves that. I'm still not confident enough in my understanding of the
  Haskell execution model to be sure what happens here, but that single `!` does
  cut the runtime in half in this case.

With the above code, the performance I get is:

```plaintext
direct (30 runs): 9.00 us (300 ns/run)
direct (3000 runs): 799.00 us (266 ns/run)
naive_ast_walk (30 runs): 63.28 ms (2109 µs/run)
naive_ast_walk (3000 runs): 7.44 s (2479 µs/run)
twe_mon (30 runs): 74.52 ms (2484 µs/run)
twe_mon (3000 runs): 6.93 s (2310 µs/run)
compile_to_closure (30 runs): 70.13 ms (2337 µs/run)
compile_to_closure (3000 runs): 8.04 s (2679 µs/run)
twe_cont (30 runs): 35.49 ms (1183 µs/run)
twe_cont (3000 runs): 3.76 s (1253 µs/run)
exec_stack (30 runs): 71.20 ms (2373 µs/run)
exec_stack (3000 runs): 8.01 s (2670 µs/run)
exec_stack_2 (30 runs): 2.73 ms (91 µs/run)
exec_stack_2 (3000 runs): 265.43 ms (88 µs/run)
```

Whether that's enough of a performance gain to justify mutable state and
possible off-by-ones will depend on context, but it's not a negligible
performance boost.

## Disabling laziness

One of the [comments on reddit][kovacs] yielded significantly faster numbers
for all of the previous approaches. You can read the comment itself, and the
linked code, for more details on the differences, but the biggest factor seems
to be that the commenter disabled laziness. (**Edit**: If I'm being honest, the
details here are a bit over my head, but he wrote a [followup] that is well
worth reading, too.) I did not know one could do that, and I think it's really
cool. In this case, we're not relying on laziness in any way, so disabling it
seems acceptable, though I'm not sure how confident I'd be about that in a real
Haskell project. Especially since the language extension is marked
"experimental".

It's per-module, though, so it may be worth having just the interpreter in a
separate, non-lazy module.

The performance benefits are important. Simply adding

```haskell
{-# Language Strict #-}
```

at the top of the file changes the above numbers to:

```plaintext
direct (30 runs): 18.00 us (600 ns/run)
direct (3000 runs): 888.00 us (296 ns/run)
naive_ast_walk (30 runs): 9.26 ms (308 µs/run)
naive_ast_walk (3000 runs): 1.02 s (339 µs/run)
twe_mon (30 runs): 24.22 ms (807 µs/run)
twe_mon (3000 runs): 2.38 s (793 µs/run)
compile_to_closure (30 runs): 9.24 ms (308 µs/run)
compile_to_closure (3000 runs): 822.09 ms (274 µs/run)
twe_cont (30 runs): 12.57 ms (418 µs/run)
twe_cont (3000 runs): 1.36 s (453 µs/run)
exec_stack (30 runs): 26.05 ms (868 µs/run)
exec_stack (3000 runs): 2.62 s (874 µs/run)
exec_stack_2 (30 runs): 2.21 ms (73 µs/run)
exec_stack_2 (3000 runs): 229.19 ms (76 µs/run)
```

Interestingly, there is not much of an impact on the optimized stack
interpreter (which makes sense: laziness and mutation don't mesh all that
well), and it remains quite a bit faster than any of the other ones. The
relative speeds of the other ones have changed quite a bit, though, so if
disabling laziness is an option in your project, that's a good one to know
about.

## Better code representation

We've kept our code as an unboxed vector of `StackOp`. It is not very difficult
to turn the code itself into an unboxed vector of `Int`: every operation but
`StackEnd` has a single argument, so there's a fairly natural mapping from
`StackOp` to pairs of integers. One can then double all of the jump targets and
increment the instruction pointer by two after each non-jump operation.

Weirdly enough, this actually made the stack interpreter a bit slower in my
experimentation (~130µs) in regular Haskell, though it did give a nice boost to
the strict version (~50µs).

That's another important thing to keep in mind when trying to optimize code:
different optimizations can sometimes interact in unexpected ways, so going for
incremental improvements by introducing one optimization at a time may not
always lead to the best overall result.

## Better opcodes

Another way to make this stack interpreter faster would be to change the design
of our stack language. For example, adding an operation that directly adds an
integer to the value on top of the stack could save us quite a bit of stack
manipulation, as our original code is pretty constant-heavy. In our compiled
stack program, we could replace the sequence

```haskell
 StackGet 0,
 StackPush 2,
 StackBin Add,
 StackPush 4,
 StackBin Add,
```

with something like:

```haskell
 StackGet 0,
 StackAddToTop 2,
 StackAddToTop 4,
```

If you have a real use-case for an interpreter (as opposed to writing a blog
post about them), and the performance requirements push you towards a
"bytecode" type of approach like a stack machine, it may be worth thinking
about the specifics of your use-case and designing the bytecode language
specifically to make your use-case fast.

## What's next?

That's all I have for now about stack machines. In the next part of this
series, we'll take a look at another type of "bytecode" interpreter, called a
register machine.

[series]: /tags/cheap%20interpreter
[part 1]: /posts/2021-06-19-cwafi-1
[part 2]: /posts/2021-06-27-cwafi-2
[part 3]: /posts/2021-07-04-cwafi-3
[part 4]: /posts/2021-07-11-cwafi-4
[part 5]: /posts/2021-07-18-cwafi-5-mea-culpa
[ndm]: https://ndmitchell.com
[cwafi]: https://www.youtube.com/watch?v=V8dnIw3amLA
[reddit]: https://www.reddit.com/r/haskell/comments/omt2yl/this_is_so_hard_to_post_why_my_last_three_blog/
[kovacs]: https://www.reddit.com/r/haskell/comments/omt2yl/this_is_so_hard_to_post_why_my_last_three_blog/h5q8ol2?utm_source=share&utm_medium=web2x&context=3
[vector]: https://hackage.haskell.org/package/vector
[followup]: https://www.reddit.com/r/haskell/comments/or74jb/two_weeks_ago_i_showed_how_to_build_a_simple_slow/h6glmsc?utm_source=share&utm_medium=web2x&context=3
