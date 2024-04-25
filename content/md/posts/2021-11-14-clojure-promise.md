{:title "The Clojure Promise"
 :layout :post
 :tags ["clojure"]}

I've recently rediscovered the Clojure `promise` construct. I don't use it very
often, but I find it very neat in its simplicity. In this post, I'll describe
what Clojure promises are (with a word on how they differ from what other
languages might call "promises"), talk about related Clojure concepts (`delay`,
`future`), and talk a bit about use-cases.

### What's a promise?

In Clojure, a promise is, quite simply, a promise to deliver a value later on.
From a more technical perspective, one can think of it as a variable that you
can already reference and talk about, but which may not yet have a value.

There are four functions that, together, form the `promise` abstraction:

- `(promise)` returns a new promise object.
- `(deliver p v)` sets the value of promise `p` to `v`. This is thread-safe,
  and should only be called once. The first call returns the promise object;
  subsequent calls return `nil` (and do not change the value of the promise).
- `(deref p)`, or more commonly `@p`, uses the generic `deref` function to get
  the value of a promise. If the value has already been delivered, it returns
  immediately; if it hasn't, this blocks indefinitely. The alternative form
  `(deref p timeout-ms timeout-val)` only waits up to `timeout-ms` milliseconds
  and, if there is no value at that point, returns `timeout-val`.
- `(realized? p)` returns `true` if the promise is already realized, which one
  could use to implement polling in cases where the blocking behaviour of
  `deref` is not desired.

And that's all there is to it. Here is a small program to show how that works:

```clojure
(defn promise-example
  []
  (let [p (promise)
        t1 (Thread. (fn []
                      (dotimes [n 5]
                        (Thread/sleep 100)
                        (print (str "T1: " @p "\n"))
                        (flush))))
        t2 (Thread. (fn []
                      (dotimes [n 5]
                        (Thread/sleep 50)
                        (print (str "T2: " (deref p 50 :timeout) "\n"))
                        (flush))))]
    (.start t1)
    (.start t2)
    (println (realized? p))
    (Thread/sleep 300)
    (deliver p 42)
    (println (realized? p))
    (.join t1)
    (.join t2)
    :done))
```

Running that function yields:

```clojure-repl
t.core=> (promise-example)
T2: :timeout
T2: :timeout
T2: 42
T1: 42
T2: 42
T1: 42
T2: 42
T1: 42
T1: 42
T1: 42
:done
t.core=>
```

I'm not aware of any other language that has such a simple definition of a
promise, but there are related constructs worth mentioning.

### Relations to other languages

Haskell has very similar [promises][haskell] in a library, though they do not
seem to support the blocking behaviour of `deref`. Not being part of `Prelude`
(or even `base`) also adds a bit of extra effort to discovering and using them.

JavaScript, of course, has been taken over by its own brand of [promises][js]
over the past few years, heralded as the answer to callback hell. In their raw
form, they're still callback-based, though, so while they help a bit with
indentation, there is still some amount of mental juggling and inversion of
control going on. That is, until you add `async` to the mix; used as the
"argument" to `await`, JavaScript promises get close to the Clojure semantics
of `deref`. While it seems like it would, in principle, be possible to extract
the `resolve` function out of a promise, the API clearly points towards
intended use closer to Clojure's `future` (see below).

Scala similarly defines its [promises][scala] as a placeholder for a value to
be delivered later, but mixes the concept with that of a future and, like
JavaScript, uses a callback-based API. It also seems to lack explicit support
for the blocking behaviour Clojure has, encouraging users to think in
asynchronous terms.

I'm not sure why no other language seem to share Clojure's simplicity on this,
but if you do know of promises in other languages, I thought it worth pointing
out that this may not be quite the same thing.

### Relations to other Clojure constructs

Clojure has a number of other "boxes" that can be "opened" with `deref` (and
with the corresponding `@` reader macro), among which:

- Atoms, refs, agents, and vars, which are all meant to be used as shared
  mutable state between threads, with various semantics. Except for the
  genericity of the `deref` function, these are different enough from a promise
  that I won't be expanding on them in this article.
- "Future" is also a name that exists in other languages, but has subtly
  different semantics. The Clojure future is best understood in terms of
  promises (as opposed to, say, in Scala, where promises are best understood in
  terms of futures), as a simple way to define, in one go, a promise and the
  thread that will fulfill it.
- Delays cannot be defined in terms of promises, but may seem similar enough
  that I think they're worth exploring a bit more here.

In Clojure, a future is a separate thread that fulfills a promise.
Schematically, we can define a future through the `future-call` function, which
could be implemented along these lines:

```clojure
(defn future-call
  [f]
  (let [p (promise)]
    (.start (Thread. (fn [] (deliver p (f)))))
    p))
```

The [real implementation][future-call] is a bit more involved (it uses
lower-level APIs than the `promise` function, uses a thread from a global
thread pool, and implements the Java Future interface for interop), but has
almost the same semantics. The main difference is that the calling code
cannot call `deliver` on the result of `future-call`, ensuring that only the
function given to `future-call` can deliver the value.

The `future` macro just wraps a thunk around its body, i.e.

```clojure
(future (do ..))
```

is the same as

```clojure
(future-call (fn [] (do ..)))
```

Delays are a bit of a middleground between promises and futures. Like the other
two, a delay represents a value that may not have been computed yet, and that
can be checked for completion with `realized?`. It is similar to a future in
the sense that the code to compute the value is fixed at the time of creating
the delay, but whereas a future immediately schedules a separate thread to
compute the value, a delay simply stores the thunk.

Calling `deref` on a delay for the first time will evaluate the stored thunk
and cache the result; all subsequent calls to `deref` return the same cached
result directly. Importantly, if `deref` (or the more specific `force`) is
never called on a `delay`, the associated computation is never performed.

Delays are created with the `delay` macro, which just takes a body as argument,
like the `future` one above.

### Channels

Promises are somewhat similar to [core.async] channels (roughly equivalent to
Haskell MVars), in that they are meant as a communication mechanism between a
producer and a consumer, generally on separate threads. The main difference is
that a promise is intended to be fulfilled once and then keep its value
constant, whereas many values are expected to be sent over a channel over time.

One could imagine building a poor man's channel with a promise that resolves to
a pair of (value, promise-for-next-pair), but in a world where [core.async]
exists, that seems a bit unnecessary.

### Uses

The documentation for `promise` says:

> Returns a promise object that can be read with `deref`/`@`, and set,
> once only, with `deliver`. Calls to `deref`/`@` prior to delivery will
> block, unless the variant of `deref` with timeout is used. All
> subsequent `deref`s will return the same delivered value without
> blocking. See also - `realized?`.

This is a good description of what a promise _is_, but is not very suggestive
as to how promises should be _used_. The most obvious use-case is as a
communication channel between two threads, but as the phrasing suggests, as lot
of those use-cases are now better served by proper [core.async] channels.

Another use that may not be so obvious is as a poor man's barrier: if you have
many threads waiting on the same promise, delivering that promise would
activate them all at once. I'm not sure I would recommend that for serious
engineering, but for quick prototypes this may be easier to use than a
[CyclicBarrier].

Finally, the use-case that reminded me of promises is to define local recursive
lazy streams. Going back to [last week's post][primes], one can compute an
infinite list of primes quite efficiently with:

```clojure
(def primes
  (cons 2
        (->> (iterate (fn [i] (+ 2 i)) 3)
             (remove (fn [n]
                       (->> primes
                            (take-while (fn [p] (<= (* p p) n)))
                            (some (fn [d] (zero? (rem n d))))))))))
```

This is a reasonably efficient definition of prime numbers: it never computes
the same prime twice, uses the sqrt trick without actually computing a
square root, and for each non-prime will stop at the first divisor found.

This is the same intention as the `get-primes-sqrt` function from [last
week][primes], but better: last week's version did compute a square root,
computed all of the divisors, and the code was much longer and arguably more
complex.

So why did I not present _this_ code last week? Well, the truth is, I hadn't
thought of it. Credit for that goes to [this Reddit comment][reddit] by
[wedesoft].

That's a very neat definition, but it does have one drawback, as discussed
[last week][primes]: it will never release its memory. Now, that may be fine
for prime numbers (they take very little memory to start with, being numbers,
and they'll never change), but remember that the goal was not to compute
primes, but to play with the language. And from that perspective, I did want to
turn that into a local seq, rather than a global one.

The obvious way to turn a global definition into a local one is to make it a
function:

```clojure
(defn get-primes
  []
  (cons 2
        (->> (iterate (fn [i] (+ 2 i)) 3)
             (remove (fn [n]
                       (->> (get-primes)
                            (take-while (fn [p] (<= (* p p) n)))
                            (some (fn [d] (zero? (rem n d))))))))))
```

but that does not work quite as well as one might hope:

```clojure-repl
t.core=> (time (nth primes 100000))
"Elapsed time: 4194.706208 msecs"
1299721
t.core=> (time (nth (get-primes) 100000))
"Elapsed time: 326617.420766 msecs"
1299721
t.core=>
```

Yes, you're reading that right: we went from ~4s to ~5.5 _minutes_.

The problem here is that, by changing from a recursive value to a recursive
function, recursive calls are now computing everything again.

I can think of a couple ways to tie that knot properly. One would be to use
memoize:

```clojure
(def get-primes-memo
  (let [h (fn [p]
            (cons 2
                  (->> (iterate (fn [i] (+ 2 i)) 3)
                       (remove (fn [n]
                                 (->> (p)
                                      (take-while (fn [p] (<= (* p p) n)))
                                      (some #(zero? (rem n %)))))))))]
    (fn [] (h (memoize get-primes-memo)))))
```

By memoizing the function we pass to `h`, we ensure that it keeps returning the
same lazy seq we're working on. This works as expected, giving us back our ~4s
performance, and allowing the memory to be reclaimed.

Memoizing a no-argument function looks a bit weird, though. Another way to
solve this is through a `promise` that we immediately deliver, using it only
for the ability to separate its declaration from its definition as a way to
work around the non-recursive semantics of `let`:

```clojure
(defn get-primes-promise
  []
  (let [p (promise)]
    (deliver
      p
      (cons 2
            (->> (iterate (fn [i] (+ 2 i)) 3)
                 (remove (fn [n]
                           (->> @p
                                (take-while (fn [p] (<= (* p p) n)))
                                (some (fn [d] (zero? (rem n d))))))))))
    @p))
```

which I think is a bit clearer than the `memoize` approach.

### Conclusion

The Clojure promise is a very simple tool, with an API of only four functions.
It's a bit of a niche tool, especially since the introduction of [core.async],
but it can still be used effectively in some instances and thus I think it's
worth knowing about.

[haskell]: https://hackage.haskell.org/package/promises-0.3/docs/Data-Promise.html
[js]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise
[scala]: https://docs.scala-lang.org/overviews/core/futures.html
[future-call]: https://github.com/clojure/clojure/blob/clojure-1.10.1/src/clj/clojure/core.clj#L6963-L6988
[core.async]: https://github.com/clojure/core.async
[CyclicBarrier]: https://docs.oracle.com/javase/7/docs/api/java/util/concurrent/CyclicBarrier.html
[primes]: /posts/2021-11-07-clj-primes
[reddit]: https://www.reddit.com/r/Clojure/comments/qowdw1/computing_prime_numbers/hk2kfy1/
[wedesoft]: https://www.reddit.com/user/wedesoft/
