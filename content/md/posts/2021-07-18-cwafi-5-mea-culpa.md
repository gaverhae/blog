{:title "Cheap interpreter, part 5: mea culpa"
 :layout :post
 :tags ["cheap interpreter"]}

[Last week][part 4], I presented the basics of a stack machine: what it is,
what a basic stack language looks like, how to write a simple interpreter for
it, and how to compile a higher-level language down to it.

This week, I had planned to talk about how to make such a stack machine _fast_,
as last week's version was slow enough to raise questions about including the
topic in a series about speed at all. This post will not be about that, though.

While trying to write a faster version, I inadvertently discovered that all of
my Haskell benchmarking was completely broken, so _that_ will be the topic for
this week.

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

### The <strike>first</strike> second clue

I completely missed the first clue. If you've read the previous parts of [this
series][series], and you haven't picked up on it, you've missed it too. It was
there all along. The second clue, the first one I _did_ notice, came after I
finished writing what I expected to be a faster version of my stack machine
interpreter from [last week][part 4]. Here is the (elided) result I got:

```plaintext
exec_stack (3 runs): 103.58 ms (34525 μs/run)
exec_stack (30 runs): 997.26 ms (33241 μs/run)
exec_stack_2 (3 runs): 162.25 ms (54082 μs/run)
exec_stack_2 (30 runs): 1.57 s (52458 μs/run)
```

In case you're unsure, `stack_exec_2` was indeed the "faster" one. There's
obviously always a chance that I screwed something up in the code itself, and
whatever I did is not, in fact, faster, and this is a legitimate result. I was
quite confident in the underlying principles, though. Much more than I was in
my ability to benchmark Haskell, which I've mentioned [before][part 3] I very
much am not. So I started thinking maybe something was a bit off.

### The third clue

I spent a bit of time playing with the code, and suddenly the results, **for
the exact same code**, became:

```plaintext
[-13,-13,-13,-13,-13,-13,-13]
direct (3 runs): 1.00 us (0 μs/run)
direct (30 runs): 1.00 us (0 μs/run)
naive_ast_walk (3 runs): 0 ns (0 μs/run)
naive_ast_walk (30 runs): 1.00 us (0 μs/run)
twe_mon (3 runs): 0 ns (0 μs/run)
twe_mon (30 runs): 0 ns (0 μs/run)
compile_to_closure (3 runs): 0 ns (0 μs/run)
compile_to_closure (30 runs): 0 ns (0 μs/run)
twe_cont (3 runs): 1.00 us (0 μs/run)
twe_cont (30 runs): 1.00 us (0 μs/run)
exec_stack (3 runs): 1.00 us (0 μs/run)
exec_stack (30 runs): 0 ns (0 μs/run)
exec_stack_2 (3 runs): 82.00 us (27 μs/run)
exec_stack_2 (30 runs): 0 ns (0 μs/run)
```

So, yeah. That completely killed all of my confidence in my benchmarking
harness. As soon as that was gone, I noticed the _first_ clue: my Clojure code
was _way_ too fast compared to my Haskell code.

I know Clojure a lot better and I was (and still am) quite confident in _those_
numbers. But Clojure compiles to JVM bytecode, and the JVM, while amazingly
performant _for a virtual machine_, is still a virtual machine running on top
of the CPU. The traditional wisdom is that each level of virtualization adds
about a 10x slow down, and while the JVM can sometimes do a bit better than
that thanks to aggressive JITing, it can't magically go _faster_ than the
underlying CPU.

I'd sort of hand-waved the discrepancy away by imagining that pehaps
Haskell is not that great at purely numerical computations, what with all the
focus on higher-order functions and types and stuff. But I really had not
digged into it, and I should have. Looking just at the baseline, the numbers I
reported in my [first benchmarks][part 3] pin Clojure code as close to _four
hundred times faster_ (2.7µs vs. 1068µs). With Haskell being a compiled
language and Clojure running on top of a VM, this should definitely have raised
some sort of alarm.

### Solving the new problem: why is it so fast?

So what's going on there? Haskell is either way too fast to measure at all, or
it's way slower than is even remotely reasonable. At that point I had two sets
of results that were both completely implausible in fundamentally different
ways. This meant they could not have the same underlying cause.

Let's start with the second set of results (Haskell being too fast), as that's
the more interesting explanation. First, remember that in [my benchmarking
code][part 3], I am _not_ setting the units for the first set of numbers. So
those zeroes are not merely the result of the reporting code rounding down: the
harness is clearly able to report nanoseconds, and my CPU is running at 2.8GHz,
so there should still be _some_ number there. (The parenthesized number _is_
fixed to microseconds, so getting a zero there is less worrisome).

The simplest explanation here is that my smarty-pants trick of making up
functions of `()` instead of plain values was not smart enough to trick the
compiler, and it still realized that all of these are indeed constant values.
Given that even the first run does not seem to take any time, it seems likely
it even inlines the values directly at compile time, which, outside of a
benchmarking context, is quite nice.

In trying to verify this hypothesis, I first changed the types of all of my
functions to be `Int -> Int` instead of `() -> Int`, but left them as `\_ ->
...`. The compiler was not impressed, and kept inlining everything, despite the
harness itself being modified to pass in a different integer for each iteration
(the iteration counter).

But there's a limit to how smart the compiler can possibly be, thanks to
[Rice's theorem][rice], so I knew it was possible to trick it. Let's take a
look at the direct calculation, for example. Here is the code I had at that
point:

```haskell
direct :: Int -> Int
direct _ =
  loop 100 1000
  where
  loop :: Int -> Int -> Int
  loop x0 i =
    if (0 == i)
    then x0
    else let x1 = x0 + 4 + x0 + 3
             x2 = x1 + 2 + 4
         in loop x2 (i - 1)
```

It's very easy for me to see that the input is not used. It's even quite easy
for the compiler, given that it forces me to name it `_`. But I can make the
code use the input without changing the semantics of the program: ultimately, I
want to do a thousand iterations, but I don't really care that my counter ends
up at zero. So here's the next version I tested:

```haskell
direct :: Int -> Int
direct n =
  loop 100 (n + 1000)
  where
  loop :: Int -> Int -> Int
  loop x0 i =
    if (n == i)
    then x0
    else let x1 = x0 + 4 + x0 + 3
             x2 = x1 + 2 + 4
         in loop x2 (i - 1)
```

Here, instead of starting my counter at `1000` and stopping at `0`, I start it
at `n + 1000` and stop at `n`. With that change, I now had a more sensible
result for that part of the benchmark:

```plaintext
direct (3 runs): 3.00 us (1 μs/run)
direct (30 runs): 20.00 us (0 μs/run)
```

This one _is_ plausibly testing the limits of what our harness can actually
measure, but the almost-10x relationship between running once and running three
times is encouraging. With similar semantically-equivalent changes, I was able
to get all of my code samples so far to produce reasonable results. I'm not
going to show all of the new code, as the changes are fairly minor, but as a
summary:

- For all of the approaches that use an environment, I know (but the compiler
  doesn't) that I only ever look at keys `0` and `1`, so I made the functions
  start with an environment where the key `13` is set to the function argument.
- For the stack machine implementation, which doesn't use an environment, I
  started the stack with the input argument. Since the correct execution of a
  stack machine operating on valid code should never try to pop the stack past
  its starting point, starting with extra content is semantically neutral.

Neither of these is _completely_ free from a performance perspective, but they
both should be negligible overall.

### Solving the old problem: why was it so slow?

This brings us to the other problem: why were my original numbers so slow? This
one is a lot less interesting and a little bit embarrassing, as that is totally
just human error, i.e. me screwing up.

I come from the Clojure world, where I can develop using a repl. This is the
best development environment I've found so far, so when I started learning
Haskell I naturally tried to find something equivalent. The nature of the
language make an exact equivalent impossible, but there is a reasonably close
approximation: [ghcid].

When developing Haskell code, I keep a window open with this code running:

```plaintext
stack exec -- ghcid -c 'stack ghci' -T 'main'
```

The `stack exec --` part in the beginning sets the context, so I'm running the
correct `ghcid` version for the current project. The `-c 'stack ghci'` part
instructs `ghcid` itself to use `stack`  to find the right version (and
configuration) of GHC for the current project. Finally, the `-T main` bit tells
`ghcid` to run the `main` function as a test.

If you don't know [ghcid], the gist of it is that it will watch the current
folder for changes and, on each change, rerun the compiler and print any error
message, or "All good". This is a big part of leveraging the compiler in active
development, as it allows the compiler to guide refactorings.

The `T` option is meant for running unit tests: if, after a change, everything
does compile correctly, instead of printing `All good` [ghcid] will execute the
(now compiled) `IO` value passed in as its argument.

All of the numbers I was looking at thus far come from that. The issue here is
that [ghcid] is designed for fast feedback during development, and as such is
using `ghci`, the incremental/interactive version of GHC. This is great for
error reporting, but it's not actually doing "real" compilation, and is thus
useless for benchmarking.

Running the code "independently" with

```plaintext
stack run
```

actually invokes the compiler to build the program (if needed) and _then_ runs
the resulting, properly compiled executable, and that's what I somewhat
accidentally did for the first time in this project last week.

### New conclusions

So, new numbers. What do they tell us? First, here they are:

```plaintext
[-13,-13,-13,-13,-13,-13,-13,-13]
direct (30 runs): 19.00 us (0 μs/run)
direct (3000 runs): 2.11 ms (0 μs/run)
naive_ast_walk (30 runs): 79.74 ms (2657 μs/run)
naive_ast_walk (3000 runs): 9.38 s (3125 μs/run)
twe_mon (30 runs): 62.18 ms (2072 μs/run)
twe_mon (3000 runs): 7.34 s (2445 μs/run)
compile_to_closure (30 runs): 73.02 ms (2434 μs/run)
compile_to_closure (3000 runs): 8.44 s (2814 μs/run)
twe_cont (30 runs): 48.92 ms (1630 μs/run)
twe_cont (3000 runs): 4.03 s (1343 μs/run)
exec_stack (30 runs): 83.99 ms (2799 μs/run)
exec_stack (3000 runs): 8.32 s (2774 μs/run)
exec_stack_2 (30 runs): 4.01 ms (133 μs/run)
exec_stack_2 (3000 runs): 290.42 ms (96 μs/run)
```

Compared to [previous benchmarking results][part 3], we can note that:

- The interpretation overhead of tree walking vs. the baseline is now in the
  3000, which is a lot larger than what we had before.
- The monadic approach is now about 30% _faster_ than the direct tree-walking,
  whereas it had previously been measured as about twice as slow. This makes
  the monadic approach a lot more viable, though the code complexity still does
  not warrant it in this case in my opinion.
- Compiling to closures still does not bring much of a benefit.
- The continuations approach is still the fastest in the simple tree walking
  category.
- The naive stack approach described [last week][part 4] is no longer three
  times slower. It's still quite a bit of extra complexity for no apparent
  speed improvement, though.

And, as expected, the "faster" stack machine approach, which I'll be covering
in the next post, is now reasonably faster than even the fastest tree-walking
approach (`twe_cont`).

[ghcid]: https://github.com/ndmitchell/ghcid
[rice]: https://en.wikipedia.org/wiki/Rice%27s_theorem
[series]: /tags/cheap%20interpreter
[part 1]: /posts/2021-06-19-cwafi-1
[part 2]: /posts/2021-06-27-cwafi-2
[part 3]: /posts/2021-07-04-cwafi-3
[part 4]: /posts/2021-07-11-cwafi-4
[ndm]: https://ndmitchell.com
[cwafi]: https://www.youtube.com/watch?v=V8dnIw3amLA
