{:title "Cheap interpreter, part 9: even faster register machines"
 :layout :post
 :tags ["cheap interpreter" "clojure"]}

[Last week][part 8] I showed a few ways in which to improve the performance of
a Haskell intepreter for a register machine. In this post, we start with the
exact same bytecode (same register language, same compiler) and show how to use
a much slower language (Clojure) to end up with a much faster interpreter.

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

## From Haskell to Clojure

We use the same design for our register machine and its compiler. The
translation to Clojure is straightforward: we omit the data definition and all
type annotations, but apart from that the code is surprisingly similar at all
levels, using [my Clojure monad macros][monad].

In order to fosu on the interesting parts, this post assumes we start with the
register code already compiled[^compile] to this form, showing Haskell and
Clojure versions side-by-side:

[^compile]: I've been using the term "the compiler" pretty loosely throughout
this series. If we take a closer look at the pipeline my code has been going
through, we generally have a first step that takes in the original code tree
and produces another tree, or list, in another language. I call that step "the
compiler". The second step, however, is not strictly speaking an interpreter,
as it consists of taking that code and returning _a function_ that, when
evaluated, will produce the result of executing the original code. In that
sense, ever since we tried continuations in [part 3], we've been essentially
using the host language runtime as our interpreter on the result of a two-pass
compiler.

```plaintext
RegState {                                           |
 num_registers = 12                                  |{:reg 2
,code = [                                            | :code
  RegLoadLiteral (Register 0) 100,                   | [[:loadl 0 100]
 ,RegLoadLiteral (Register 1) 1000                   |  [:loadl 1 1000]
 ,RegBin NotEq (Register 3) (Register 2) (Register 1)|  [[:bin :not=] 3 2 1]
 ,RegJumpIfZero (Register 3) 11                      |  [:jump-if-zero 3 11]
 ,RegBin Add (Register 5) (Register 0) (Register 4)  |  [[:bin :add] 5 0 4]
 ,RegBin Add (Register 6) (Register 5) (Register 0)  |  [[:bin :add] 6 5 0]
 ,RegBin Add (Register 0) (Register 6) (Register 7)  |  [[:bin :add] 0 6 7]
 ,RegBin Add (Register 9) (Register 0) (Register 8)  |  [[:bin :add] 9 0 8]
 ,RegBin Add (Register 0) (Register 9) (Register 10) |  [[:bin :add] 0 9 10]
 ,RegBin Add (Register 1) (Register 11) (Register 1) |  [[:bin :add] 1 11 1]
 ,RegJump 2                                          |  [:jump 2]
 ,RegEnd (Register 0)]                               |  [:return 0]]
,hoisted = Env (fromList [                           | :hoisted
  (2,0)                                              | {2 0
 ,(4,4)                                              |  4 4
 ,(7,3),(8,2)                                        |  7 3
 ,(10,4)                                             |  8 2
 ,(11,-1)])}                                         |  11 }}
```

We could write the same interpreter in Clojure as we did in Haskell. That
yields a runtime of about 180µs, or about three times slower than Haskell. This
is in line with pretty much all of the approaches we've seen so far: the
Clojure version, as a direct translation, is about three to six times
slower.[^tce]

[^tce]: Except for the continuations approach which is much slower as Clojure
  does not do tail call elimination, and the "compile to closure" approach
  which is slightly faster in Clojure, probably because the strict nature of
  the language makes it able to optimize that better. (This is comparing to
  "normal", i.e. lazy, Haskell performance.)

So what can we do in Clojure that we cannot do in Haskell, and results in such
a big speedup that it actually makes the Clojure version faster? Glad you
asked.

## "Cheap" restriction

First, a clarification on that grand announcement: it's not that this is
_impossible_ to do in Haskell, but trying to replicate this technique in
Haskell is definitely going to break our self-imposed notion of what makes an
interpreter "cheap", i.e. roughly speaking "easy to maintain for people who
don't know another programming language than the one the interpreter is written
in".

## Host interpreter

Clojure is a _dynamic language_. I'm deliberately not saying _dynamically
typed_ here, because that misses the point. The point of being dynamic is not
to get _rid_ of static types (static types are good!), but to get _access_ to
`eval`. In other words, a dynamic language is one in which it is _easy_ (i.e.
_cheap_) to load new code at runtime, because the host language interpreter is
available.

If we are ourselves writing an interpreter, having our host language
interpreter suggests a different approach: what if instead of writing an
interpreter, we wrote a compiler to our host language, and then used our host
language interpreter to generated "native" (in our itnerpreted world) code
instead?

The main obstacle to doing just that is that, in most dynamic languages, `eval`
takes in a string, and generating synractically valid code strings is a pain
and usually does not compose all that well.

However, Clojure is a repl-based language, which means it understands how
important the separation between `read` (the `r` in repl) and `eval` (the `e`
in repl) is, and therefore the `eval` we get access to reads in Clojure code,
represented as Clojure data structures.

Manipulating Clojure data structures is what Clojure was built for[^data], so
generating Clojure code from a Clojure program is very easy. Turning that code
into a runnable function is just one `eval` call away.

[^data]: It may seem like an empty truism at first, but if you think about it
  for a bit you'll realize "manipulating \<Language X\> data structures is what
  \<Language X\> was built for" is far from being true for all values of
  \<Language X\>.

All that's left for us to do now is think about exactly _what_ code we're going
to generate.



[series]: /tags/cheap%20interpreter
[part 1]: /posts/2021-06-19-cwafi-1
[part 2]: /posts/2021-06-27-cwafi-2
[part 3]: /posts/2021-07-04-cwafi-3
[part 4]: /posts/2021-07-11-cwafi-4
[part 5]: /posts/2021-07-18-cwafi-5-mea-culpa
[part 6]: /posts/2021-07-25-cwafi-6-faster-stack-machine
[part 7]: /posts/2021-08-01-cwafi-7-register-machine
[part 8]: /posts/2021-08-08-cwafi-8-fast-registers
[ndm]: https://ndmitchell.com
[cwafi]: https://www.youtube.com/watch?v=V8dnIw3amLA
[monad]: /posts/2021-05-16-monads-5#monads-in-clojure
