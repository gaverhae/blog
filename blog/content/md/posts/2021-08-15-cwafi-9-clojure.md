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

In order to focus on the interesting parts, this post assumes we start with the
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
 ,(11,-1)])}                                         |  11 -1}}
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
interpreter available suggests a different approach: what if instead of writing
an interpreter, we wrote a compiler to our host language, and then used our
host language interpreter to generate "native" (in our interpreted world) code
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

## Instruction pointer

Let's take a closer look at the [array-based interpreter from last week][part
8].  We've gone through quite a bit of effort there to minimize the work done
in each step, to the point where it's basically just an array lookup, a write,
or a combination of both.

We're also incrementing a loop counter and looping back after every single
instruction. Normally, for most code, that would be completely negligible.
However, because each of our instructions is so small, the looping construct is
not free here.

There isn't really a great way around that as long as we're writing and
interpreter ourselves, but if we're coopting our host language interpreter, we
can instead use our host language instruction pointer too. This can be done by
sequencing multiple instructions as one block of (generated) code.

Ideally, we'd want to generate a single block of code for our host language
interpreter. But since our source language (in this context, the register
machine language) has jumps, that may not be possible.

## Jumps

The Clojure language does not have `goto`, so we can't translate those `:jump`
instructions directly. Instead, we'll start by identifying the possible
entrypoints in our code, and generate a separate block of code for each.

In order to identify possible entrypoints, we first need to be able to find all
of the jump instructions, as they are pointing to the points at which we may
enter the code:

```clojure
(def is-jump? (comp #{:jump :jump-if-zero} first))
```

Now, in order to find all of the entrypoints, we can simply walk over all
instructions (with their index), filter the ones that are jumps, and look at
where they're jumping. For simplicity, we want a segment of code to end on a
jump instruction, which means that `:jump-if-zero` has to always jump, just
like `:jump` does. In order to not change the semantics of our code, we'll make
it a conditional choice between two jump targets: the address written into the
instruction, and the address right after the instruction. This is why we need
to know the index of the jump instruction itself.

```clojure
(defn find-entrypoints
  [code]
  (->> code
       (map-indexed vector)
       (filter (comp is-jump? second))
       (mapcat (fn [[idx op]]
                 (match op
                   [:jump x] [x]
                   [:jump-if-zero _ x] [x (inc idx)])))
       (cons 0)
       set
       sort))
```

Run on our register code, this yields:

```clojure-repl
t.core=> (find-entrypoints (:code (compile-register ast)))
(0 2 4 11)
t.core=>
```

Next, we want to use that information to actually cut out each code segment,
i.e. all of the code between each entrypoint and the first jump it reaches:

```clojure
(defn find-segments
  [code]
  (->> code
       find-entrypoints
       (map
         (fn [ep]
           [ep (->> (drop ep code)
                    (reduce (fn [acc op]
                              (if (is-jump? op)
                                (reduced (conj acc op))
                                (conj acc op)))
                            []))]))))
```

`reduced` is a little-known and rarely useful function that wraps its argument
in a special container that the outer `reduce` call knows to look for, and that
stops the reduction early. Hence this will pick the sublist of code that starts
at the entrypoint, included, and stops at the first jump, included.

```clojure-repl
t.core=> (find-segments (:code (compile-register ast)))
([0 [[:loadl 0 100]
     [:loadl 1 1000]
     [[:bin :not=] 3 2 1]
     [:jump-if-zero 3 11]]]
 [2 [[[:bin :not=] 3 2 1]
     [:jump-if-zero 3 11]]]
 [4 [[[:bin :add] 5 0 4]
     [[:bin :add] 6 5 0]
     [[:bin :add] 0 6 7]
     [[:bin :add] 9 0 8]
     [[:bin :add] 0 9 10]
     [[:bin :add] 1 11 1]
     [:jump 2]]]
 [11 [[:return 0]]])
t.core=>
```

## Generating Clojure code

The next step in assembling our little compiler is to turn each of these segments into a block of Clojure code.

```clojure
(defn compile-segment-arr
  [[ep segment] hoisted registers]
  (let [re-get (fn [r] `(long ~(hoisted r `(aget ~registers (int ~r)))))]
    [ep (->> segment
             (map (fn [op]
                    (match op
                      [:return r] (re-get r)
                      [:loadl r v] `(aset ~registers (int ~r) (long ~v))
                      [:loadr to from] `(aset ~registers (int ~to)
                                              ~(re-get from))
                      [:jump-if-zero r to] `(if (zero? ~(re-get r))
                                              (recur (int ~to))
                                              (recur (int ~(+ ep
                                                              (count segment)))))
                      [:jump to] `(recur (int ~to))
                      [[:bin :add] to r1 r2] `(aset ~registers (int ~to)
                                                    (unchecked-add
                                                      ~(re-get r1)
                                                      ~(re-get r2)))
                      [[:bin :not=] to r1 r2] `(aset ~registers (int ~to)
                                                     (if (== ~(re-get r1)
                                                             ~(re-get r2))
                                                       0 1)))))
             (cons 'do))]))
```

In case you're not very familiar with the specifics of Clojure syntax, backtick
starts a quasiquote in which tilde escapes. For example, given the `:hoisted`
value from our register machine code and a `registers` value of `regs`, the
call `(re-get 5)` will yield the list `'(long (aget regs (int 5)))`, whereas
`(re-get 2)` would yield `(long 0)`.

This function expects `[ep segment]` to be the values returned from
`find-segments`, `hoisted` to be the integer-to-integer map returned by our
`compile-registers` call, and `registers` to be a Clojure symbol, i.e.
something that can play the role of a variable once the code gets compiled.

For example, called on our first segment, this yields (in a Clojure repl, `*1`
is bound to the result of the last printed value):

```clojure-repl
t.core=> (find-segments (:code (compile-register ast)))
([0 [[:loadl 0 100]
     [:loadl 1 1000]
     [[:bin :not=] 3 2 1]
     [:jump-if-zero 3 11]]]
 [2 [[[:bin :not=] 3 2 1]
     [:jump-if-zero 3 11]]]
 [4 [[[:bin :add] 5 0 4]
      [[:bin :add] 6 5 0]
      [[:bin :add] 0 6 7]
      [[:bin :add] 9 0 8]
      [[:bin :add] 0 9 10]
      [[:bin :add] 1 11 1]
      [:jump 2]]]
 [11 [[:return 0]]])
t.core=> (compile-segment-arr (first *1) {2 0, 4 4, 7 3, 8 2, 11 -1} 'regs)
[0 (do (clojure.core/aset regs
                          (clojure.core/int 0)
                          (clojure.core/long 100))
       (clojure.core/aset regs
                          (clojure.core/int 1)
                          (clojure.core/long 1000))
       (clojure.core/aset regs
                          (clojure.core/int 3)
                          (if (clojure.core/==
                                (clojure.core/long 0)
                                (clojure.core/long
                                  (clojure.core/aget regs
                                                     (clojure.core/int 1))))
                            0
                            1))
       (if (clojure.core/zero?
             (clojure.core/long
               (clojure.core/aget regs
                                  (clojure.core/int 3))))
         (recur (clojure.core/int 11))
         (recur (clojure.core/int 4))))]
t.core=>
```

## Compiling Clojure code

The only step left is to assemble all the segments into one big block of code,
somehow. We'll do that by running a loop around them, with an instruction
pointer. We started with a goal of avoiding this, but ultimately this is a
pretty cheap way to switch between blocks of code. What matters is that now
each of those blocks is big enough that the management of the instruction
pointer is noise again.

We need to introduce two Clojure bindings in the form of symbols: one to
hold the instruction pointer, and one to hold the register array. Apart from
that, we simply lay down all the segments as the body for the loop. The `~@`
notation expands the following form and inlines it, removing one level of list
wrapping.

```clojure
(defn compile-rc-arr
  [{:keys [code hoisted reg]}]
  (let [registers (gensym)
        ip (gensym)]
    `(fn []
       (let [~registers (long-array ~(inc reg))]
         (loop [~ip (int 0)]
           (case ~ip
             ~@(->> (find-segments code)
                    (mapcat #(compile-segment-arr % hoisted registers)))))))))
```

Applied on our example code, and with a little bit of clean-up for readability
(i.e. removing all the `clojure.core/` prefixes as there is no ambiguity here),
this yields:

```clojure
(fn []
  (let [G__8219 (long-array 13)]
    (loop [G__8220 (int 0)]
      (case G__8220
        0 (do (aset G__8219 (int 0) (long 100))
              (aset G__8219 (int 1) (long 1000))
              (aset G__8219
                    (int 3)
                    (if (== (long 0) (long (aget G__8219 (int 1))))
                      0
                      1))
              (if (zero? (long (aget G__8219 (int 3))))
                (recur (int 11))
                (recur (int 4))))
        2 (do (aset G__8219
                    (int 3)
                    (if (== (long 0) (long (aget G__8219 (int 1))))
                      0
                      1))
              (if (zero? (long (aget G__8219 (int 3))))
                (recur (int 11))
                (recur (int 4))))
        4 (do (aset G__8219
                    (int 5)
                    (unchecked-add (long (aget G__8219 (int 0)))
                                   (long 4)))
              (aset G__8219
                    (int 6)
                    (unchecked-add (long (aget G__8219 (int 5)))
                                   (long (aget G__8219 (int 0)))))
              (aset G__8219
                    (int 0)
                    (unchecked-add (long (aget G__8219 (int 6)))
                                   (long 3)))
              (aset G__8219
                    (int 9)
                    (unchecked-add (long (aget G__8219 (int 0)))
                                   (long 2)))
              (aset G__8219
                    (int 0)
                    (unchecked-add (long (aget G__8219 (int 9)))
                                   (long 4)))
              (aset G__8219
                    (int 1)
                    (unchecked-add (long -1)
                                   (long (aget G__8219 (int 1)))))
              (recur (int 2)))
        11 (do (long (aget G__8219 (int 0))))))))
```

I'm not going to try and claim this is good code by any metric, but it is
recognizably the same as our register code. Turning this into a Clojure
function is just a matter of running `eval` on this list:

```clojure-repl
t.core=> (eval (compile-registers-arr (compile-register ast)))
#object[t.core$eval8243$fn__8244 0x4c4dcaad "t.core$eval8243$fn__8244@4c4dcaad"]
t.core=> (*1)
-13
t.core=>
```

## Next step

Yes, "step" singular. Probably.

Benchmarking that generated code on my laptop yields a runtime of about 32ms,
which is, as claimed, faster than the Haskell code I presented [last week][part
8]. It's not _much_ faster, though. We can do quite a bit better, and I'll
explain how next week, in what I hope will be the last entry in this
series.[^last]

[^last]: I really expected this one to be the last, but it turns out I had a
  bit more to say than I thought, and this post is long enough already.
  Hopefully I'll be able to fit all I still want to say in a single post next
  week, as I'm looking forward to write about something else for a change.
  Also, 10 is a nice, round number.

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
