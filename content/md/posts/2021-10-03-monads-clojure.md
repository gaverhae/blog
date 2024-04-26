{:title "Monads for Clojure programmers"
 :layout :post
 :tags ["clojure"]}

This post is strictly about the programming technique. There is no philosophy
or category theory. If you're interested in a more in-depth explanation of all
the concepts related to monad, I have a [longer series on the subject][monad].

My goal with this post is not to convince you to start using monads everywhere;
I only aim to make the technique easy to understand, using Clojure notation to
explain it.

### What is it for?

Monads are good for reifying side-effects while keeping your code pure. A
single monad can reify all possible side-effects (like the `IO` monad in
Haskell), or you can make a small, bespoke monad to reify just the one
side-effect you want to allow in a specific context.

When spelled-out, monadic values (concrete instances of a specific monad) will
look like an imperative DSL, with each line binding the return value of a
side-effecting "operation" to a variable name. When you define a monad, you
define the set of valid operations (i.e. effects).

All monads need two "special" monadic values: one that represents no operation
(i.e. no side-effect, i.e. a pure computation), usually called "pure" or
"return", and one that represents the effect of chaining together two effects,
usually called "bind" (written `>>=` in Haskell, in case you've ever looked
that way).

A monad with just those two is (usually) not very useful, so in general you
will add more.

### What makes a monad

As promised, no category theory definition. For our purposes, a monad is made
out of:

- A data type with multiple _variants_, amongst which at least `:return` and
  `:bind`. We will refer to values of this type as "monadic values".
- A function that can "run" monadic values, with well-defined semantics for
  `:return` and `:bind`, and pretty much arbitrary semantics for all of the
  other variants. In the rest of this post, we'll refer to this function as
  "the `run` function".

By _variant_ here we mean vectors where the first element is a keyword, and
where the number and types of the other elements are defined by that keyword.

The `:return` variant has one element of any type; the `:bind` variant has two
elements, where the first one is a monadic value and the second one is a
function that takes a "normal" (non-monadic) value and returns a monadic value.

The `run` function applied to `:bind` must have the semantics of "unwrapping"
the monadic value and applying its function to it.

Because Clojure is a dynamic language, and because the `run` function will be
written as a case analysis on the type of its argument(s), we can fully
formalize a specific monad by just specifying its `run` function.

### A bit of syntax

There is a reason why most monad tutorials use Haskell: without syntactic
support, monads are pretty clunky to use. Clojure does not have syntactic
support out of the box, but, being a lisp, it has macros. This means we can add
syntactic support, in the form of two macros:

```clojure
(defmacro match
  [expr & cases]
  (let [e (gensym)]
    `(let [~e ~expr]
       (case (first ~e)
         ~@(->> (partition 2 cases)
                (mapcat (fn [[pat body]]
                          [(first pat)
                           `(let [~(vec (cons '_ (rest pat))) ~e]
                              ~body)])))))))

(defmacro mdo
  [bindings]
  (if (#{0 1} (count bindings))
    (throw
      (RuntimeException. "invalid number of elements in mdo bindings"))
    (let [[n v & r] bindings]
      (if (empty? r)
        v
        [:bind v `(fn [~n] (mdo ~r))]))))
```

Why we might want them will be clarified soon enough; for now, let's look at
what they _do_.

First, the `match` macro will allow us to choose between a number of
alternatives based on the first element of a vector. Here is an example
expansion:

```clojure-repl
t.core=> (macroexpand-1 '(match expr
                           [:lit v] [v env]
                           [:var idx] [(get env idx) env]
                           [:set idx e] (let [[v env] (h e env)]
                                          [v (assoc env idx v)])))
(clojure.core/let [G__2018 expr]
  (clojure.core/case (clojure.core/first G__2018)
    :lit (clojure.core/let [[_ v] G__2018]
           [v env])
    :var (clojure.core/let [[_ idx] G__2018]
           [(get env idx) env])
    :set (clojure.core/let [[_ idx e] G__2018]
           (let [[v env] (h e env)]
             [v (assoc env idx v)]))))
t.core=>
```

We match on the first element of a vector, and bind the other elements to the
symbols we give. In other words, this is a specialized, poor man's
[core.match]. We could of course also just use core.match, but because we don't
need its full power here, I don't want to import its full complexity.

This will be very useful in writing the `run` function, as the core of what it
does is dispatching on its first argument.

Next, let's take a look at `mdo`. Here is an example use:

```clojure-repl
t.core=> (macroexpand-1 '(mdo [a [:return 5]
                               b [:return 3]
                               c [:return (+ a b)]
                               _ [:set 0 c]
                               v [:get 2]
                               _ [:return (+ v c)]]))
[:bind [:return 5] (clojure.core/fn [a]
                     (t.core/mdo (b [:return 3]
                                  c [:return (+ a b)]
                                  _ [:set 0 c]
                                  v [:get 2]
                                  _ [:return (+ v c)])))]
t.core=>
```

`macroexpand-1` only does one round of macro expansion, so it's not all that
useful for a recursive macro. Unfortunately, a full `macroexpand` is not a
whole lot more useful:

```clojure-repl
t.core=> (macroexpand (mdo [a [:return 5]
                          b [:return 3]
                          c [:return (+ a b)]
                          _ [:set 0 c]
                          v [:lookup 2]
                          _ [:return (+ v c)]]))
[:bind
 [:return 5]
 #object[t.core$eval1756$fn__1757 0x2910aca
         "t.core$eval1756$fn__1757@2910aca"]]
t.core=>
```

So, you'll have to trust me a bit on this one, but here's the result of a
manual recursive expansion of the macro:

```clojure
[:bind [:return 5]
 (fn [a]
   [:bind [:return 3]
    (fn [b]
      [:bind [:return (+ a b)]
       (fn [c]
         [:bind [:set 0 c]
          (fn [_]
            [:bind [:lookup 2]
             (fn [v]
               [:return (+ v c)])])])])])]
```

The semantics we want here is that the `run` function will, when running on a
`[:bind ma f]` value, first fully evaluate `ma`, then run the function `f` with
the result of evaluating `ma` as the argument. This may be a bit clearer with
an example.

### A first example: ambient state

Let's make all of that a little bit more concrete. We start by defining an
"ambient state" monad, one in which you can, at any point, reach out to a
"global variable" that maintains some piece of state.

Specifically, we want to reify two effects:

- `[:get k]` will return the value associated to the key `k`.
- `[:set k v]` will set the value associated to key `k` to value `v`.

Here is the definition of this monad, in the form of a `run` function:

```clojure
(defn run-ambient
  ([m] (run-ambient m {}))
  ([m env]
   (match m
     [:return v] [v env]
     [:bind ma f] (let [[v env] (run-ambient ma env)]
                    (run-ambient (f v) env))
     [:set k v] [v (assoc env k v)]
     [:get k] [(get env k) env])))
```

There's nothing very complicated about this code: we just keep around some
state, in this case a map that represents our ambient state.

Applying that function to our earlier example of a monadic value yields:

```clojure-repl
t.core=> (run-ambient
           (mdo [a [:return 5]
                 b [:return 3]
                 c [:return (+ a b)]
                 _ [:set 0 c]
                 v [:get 2]
                 _ [:return (+ v c)]])
           {2 3})
[11 {2 3, 0 8}]
t.core=>
```

That's not very interesting, though. A more realistic use-case would involve
calling functions on the right-hand side of these assignments. That would
showcase a bit more of the power of this approach: our ambient state would
still be available in those subfunctions.

As a more complex example, here is a bit of code that counts, for each type of
expression, the number of times it appears in a program, for a simple AST. It
also returns the total number of expression nodes in the program.

```clojure
(defn sequenceM
  "Takes a list of monadic values ms, and returns
   a single monadic value that wraps a list."
  [ms]
  (if (empty? ms)
    [:return ()]
    (mdo [a (first ms)
          r (sequenceM (rest ms))
          _ [:return (cons a r)]])))

(defn update
  [k f]
  (mdo [v [:get k]
        _ [:set k (f v)]]))

(defn count-exprs
  [expr]
  (let [finc (fnil inc 0)]
    (match expr
      [:lit e] (mdo [_ (update :lit finc)
                     _ [:return 1]])
      [:var _] (mdo [_ (update :var finc)
                     _ [:return 1]])
      [:set _ e] (mdo [c (count-exprs e)
                       _ (update :set finc)
                       _ [:return (inc c)]])
      [:bin _ e1 e2] (mdo [c1 (count-exprs e1)
                           c2 (count-exprs e2)
                           _ (update :bin finc)
                           _ [:return (+ c1 c2 1)]])
      [:while e-cond e-body] (mdo [c1 (count-exprs e-cond)
                                   c2 (count-exprs e-body)
                                   _ (update :while finc)
                                   _ [:return (+ c1 c2 1)]])
      [:do & exprs] (mdo [counts (sequenceM (mapv count-exprs exprs))
                          _ (update :do finc)
                          _ [:return (reduce + 1 counts)]]))))
```

Note that `sequenceM` will work with any monad, while `update` is more specific
to this one.

Running this yields:

```clojure-repl
t.core=> (run-ambient
           (count-exprs
             [:do
              [:set 0 [:lit 100]]
              [:set 1 [:lit 1000]]
              [:while
               [:bin :not= [:lit 0] [:var 1]]
               [:do
                [:set 0 [:bin :add [:bin :add [:bin :add [:var 0] [:lit 4]]
                                    [:var 0]]
                         [:lit 3]]]
                [:set 0 [:bin :add [:bin :add [:var 0] [:lit 2]]
                         [:lit 4]]]
                [:set 1 [:bin :add [:lit -1] [:var 1]]]]]
              [:var 0]]))
[29 {:lit 8, :set 5, :var 6, :bin 7, :do 2, :while 1}]
t.core=>
```

An interesting note to make about `count-exprs` is that there is no explicit
threading of state through the recursive calls. The code reads as imperative,
mutable code, while there is in fact no mutation going on.

### Second example: non-deterministic computation

Monads are not just about state management: they can also be used to change the
computation model. As a simple example, here is a monad that allows every
(non-pure) computation to return multiple results, and runs all the results
through the rest of the computation:

```clojure
(defn run-nd
  ([ma] (run-nd ma []))
  ([ma s]
   (match ma
     [:return a] [a]
     [:bind ma f] (mapcat (comp run-nd f) (run-nd ma))
     [:multi ls] ls)))
```

Here is an example run, where we compute all four possible permutations:

```clojure-repl
t.core=> (run-nd (mdo [a [:multi [1 2]]
                       b [:multi [3 4]]
                       _ [:return (+ a b)]]))
(4 5 5 6)
t.core=>
```

We can make special operators for this monad. For example, we could make a
filter that only returns positive values:

```clojure
(defn filter-pos
  [a]
  [:multi (if (pos? a) [a] [])])
```

We can run that filter like this:

```clojure-repl
t.core=> (run-nd (mdo [a [:multi [-1 2]]
                       b [:multi [3 4]]
                       c [:return (* a b)]
                       d (filter-pos c)
                       _ [:return d]]))
(6 8)
t.core=>
```

A simple possible use-case would be the computation of the solution of
quadratic equations:

```clojure
(defn sqrt
  [x]
  (cond (neg? x)  [:multi []]
        (zero? x) [:return 0]
        (pos? x)  (mdo [sign [:multi [1 -1]]
                        _    [:return (* sign (Math/sqrt x))]])))

(defn div
  [a b]
  (if (zero? b)
    [:multi []]
    [:return (/ a b)]))

(defn solve-2nd
  [a b c]
  (mdo [d (sqrt (- (* b b) (* 4 a c)))
        x (div (- d b)
               (* 2 a))]))
```

where we define `sqrt` as a function that returns both roots for positive
numbers, and none for negative ones. Similarly, `div` will return no solution
for division by 0. This propagates directly to our solver, which will return
zero, one or two answers:

```clojure-repl
t.core=> (run-nd (solve-2nd 1 2 1))
(-1)
t.core=> (run-nd (solve-2nd 1 3 1))
(-0.3819660112501051 -2.618033988749895)
t.core=> (run-nd (solve-2nd -2 0 -1))
()
```

Just like there was no explicit threading of state through the body of
`count-exprs`, there is no explicit handling of multiple values in the body of
`solve-2nd`.

### What about category theory?

In order for a monad to be useful, the behaviour of `:return` and `:bind` need
to follow a set of rules, generally known as "the monad laws" (sometimes
"monadic" laws). In practice, it's pretty hard to violate the laws and still
end up with something useful. Expressed in Clojure (and using the conventions
described in this post), the laws would be:

```clojure
(and
  (= (run [:bind [:return a] f])
     (run (f a)))
  (= (run [:bind m (fn [x] [:return x])])
     (run m))
  (= (run [:bind [:bind m g] f])
     (run [:bind m (fn [x] [:bind (g x) f])])))
```

Category theory gives a mathematical justification for those laws. That's it.

### Conclusion

This only scratched the surface. Similarly to the first example, you could
define a monad that represents the effect of interacting with your database.
You could then define two `run` functions for it: one that actually interacts
with the database, and one, used for testing, that only simulates those
interactions. Your production code would still be the exact same `mdo`
expression. You could define a monad where the `run` function is expected to
receive multiple monadic values, and runs them concurrently, one `:bind` step
at a time. This could give you green threads, possible with some
synchronization mechanism à la [core.async] channels, in surprisingly little
code.

Despite their power, monads are not used very much in Clojure. This is mainly
because we have other, more familiar alternatives. The kind of state management
presented in the first example can easily be achieved through passing around an
`atom`, or actually using mutable ambient state in the form of a dynamic
variable. The database use-case can be achieved through passing around an
instance of a protocol, for which you'd have two implementations. Changing the
computational model can be done through macros, as done in [core.async] and
[core.logic].

Ultimately, how much monads seem appealing to you will depend on how much you
care about avoiding mutable state. I believe there is a place for mutable
state, but there are cases where it's better to avoid it, and knowing about
monads gives you one more technique for that.

[monad]: /tags/monad-tutorial
[core.match]: https://github.com/clojure/core.match
[core.logic]: https://github.com/clojure/core.logic
[core.async]: https://github.com/clojure/core.async
