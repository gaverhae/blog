{:title "Computing prime numbers with Clojure"
 :layout :post
 :tags ["clojure" "primes"]}

This post is not about prime numbers, really, but they offer a nice, compact
problem space to explore a number of Clojure language features.

We'll start with defining what prime numbers are, using a naïve Clojure
implementation. Then, we'll discuss the performance characteristics of that
implementation, and explore various ways in which that can be improved.

### Prime numbers

Prime numbers are integers, so the first thing we need is a list of all
integers:

```clojure
(def integers
  (cons 1
        (lazy-seq (map inc integers))))
```

A common definition of a prime number is "a positive integer that is divisible
by itself and one, and by no other positive integer".

A direct translation of this definition would first compute the list of
divisors, and then compare that to the list `[1 n]`:

```clojure
(defn divisors
  [n]
  (->> integers
       (take-while (fn [i] (<= i n)))
       (filter (fn [i] (zero? (rem n i))))))

(defn prime?
  [n]
  (= [1 n] (divisors n)))
```

We can now define the list of all prime numbers:

```clojure
(def primes
  (filter prime? integers))
```

and compute the nth prime number:

```clojure-repl
t.core=> (nth primes 100)
547
t.core=>
```

### Performance

The above definitions are correct from a mathematical perspective, and seem to
work at first:

```clojure-repl
t.core=> (time (nth primes 0))
"Elapsed time: 0.344016 msecs"
2
t.core=> (time (nth primes 10))
"Elapsed time: 0.943427 msecs"
31
t.core=> (time (nth primes 100))
"Elapsed time: 56.789246 msecs"
547
t.core=>
```

But that last one seems a bit slow, doesn't it? Let's push a bit further.

```clojure-repl
t.core=> (time (nth primes 1000))
"Elapsed time: 7613.744297 msecs"
7927
t.core=>
```

That looks like we're taking about one millisecond _per integer_ we filter
through. That's definitely not good. This is a bit hard to benchmark, though,
because:

```clojure-repl
t.core=> (time (nth primes 1000))
"Elapsed time: 0.201189 msecs"
7927
t.core=>
```

### Bounding memory use

So what's going on? Let's first look at `primes`. By making it a global lazy
list, we get memoization for free. This can be good, or it can be terrible.
Mainly, it means that, once we have computed prime numbers up to the nth, we
keep that _for the entire life of our program_. Here, it's a list of numbers,
so it's not taking up a lot of memory, but in general one has to be a bit
careful when defining global infinite lists.

It's usually a better practice to define a function that returns a lazy list
instead, so that callers can scope the result. For example:

```clojure
(defn get-primes
  []
  (filter prime? integers))
```

In this case, the caller of `get-primes` can choose how much caching to do, and
for how long they want to hold on to the list. This is generally a better
approach, as the caller is in a better position to know how much reuse they
want to get out of that one list vs. how much memory they're willing to
allocate it. (Note that `integers` is similarly defined as a global memory
hog.)

Specifically, in this case, when `nth` returns, the memory allocated
for the list returned by `get-primes` is cleared. The tradeoff is that when we
want to call `get-primes` again, we don't have any cache to rely on.

```clojure-repl
t.core=> (time (nth primes 1000))
"Elapsed time: 7644.215077 msecs"
7927
t.core=> (time (nth primes 1001))
"Elapsed time: 12.338383 msecs"
7933
t.core=> (time (nth primes 999))
"Elapsed time: 0.200195 msecs"
7919
t.core=> (time (nth (get-primes) 1000))
"Elapsed time: 7453.266664 msecs"
7927
t.core=> (time (nth (get-primes) 1001))
"Elapsed time: 7299.552746 msecs"
7933
t.core=> (time (nth (get-primes) 999))
"Elapsed time: 7289.87252 msecs"
7919
t.core=>
```

Which approach is "best" will depend a lot on the application, but it's worth
observing that, if you define `get-primes`, consuming code can decide how much
caching to do, including defining a global `(def primes (get-primes))` if that
fits their use-case. However, if you define a global list like `primes` above,
there's no possible choice for your users.

### Algorithmic optimizations

Prime numbers can be computed a bit faster by keeping the same general
principles, but introducing two mathematical tricks in looking for divisors:

- Divisors are "symmetric" around \\(\\sqrt\{n\}\\): if `(/ n a)` is `b`,
  then `(/ n b)` is `a`, except when `(= a b)`, in which case they're both
  equal to the square root. This means that in testing for divisors, we don't
  need to go all the way up to `n`, but can stop at \\(\\sqrt\{n\}\\). For large
  numbers, that's a big reduction.
- For a number to be prime, it is enough that it does not have any _prime_
  divisor, so we can test only for prime numbers rather than all integers.

This means that, in order to compute whether a new number is prime, we need to
know all of the previous primes, or at least the ones smaller than
\\(\\sqrt\{n\}\\). We're going to introduce a little bit of gymnastics here to
keep that around: we're using `iterate`, which repeatedly calls a function on
the result of calling it. This is a great function to generate infinite lazy
sequences based on some state. For example, one can compute Fibonacci numbers
like this:

```clojure-repl
t.core=> (->> [1 1]
              (iterate (fn [[a b]] [b (+ a b)]))
              (map first)
              (take 10))
(1 1 2 3 5 8 13 21 34 55)
t.core=>
```

The difficulty is, in general, in defining the state and how to extract the
result from it. Here, the state is the current number and the next one, and we
extract by keeping the current number.

To compute primes, we need a slightly more complicated state. We'll use a
triplet where the first element is the number we need to check next, the second
element is the vector of primes we've found so far, and the last element is a
boolean indicating whether we just added a new prime or not.

```clojure
(defn get-primes-sqrt
  []
  (letfn [(divisors [n primes]
            (let [max (Math/sqrt n)]
              (->> primes
                   (take-while (fn [i] (<= i max)))
                   (filter (fn [i] (zero? (rem n i)))))))
          (prime? [n primes]
            (empty? (divisors n primes)))]
    (->> [3 [2] true]
         (iterate (fn [[n primes-so-far added?]]
                    (let [add? (prime? n primes-so-far)]
                      [(inc n)
                       (if add? (conj primes-so-far n) primes-so-far)
                       add?])))
         (filter #(get % 2))
         (map (comp peek second)))))
```

This is vastly faster:

```clojure-repl
t.core=> (time (nth (get-primes) 1000))
"Elapsed time: 7760.343331 msecs"
7927
t.core=> (time (nth (get-primes-sqrt) 1000))
"Elapsed time: 14.870077 msecs"
7927
t.core=>
```

Let's get a feel for how much faster this is:

```clojure-repl
t.core=> (time (nth (get-primes-sqrt) 10000))
"Elapsed time: 223.990175 msecs"
104743
t.core=> (time (nth (get-primes-sqrt) 100000))
"Elapsed time: 4901.714288 msecs"
1299721
t.core=> (time (nth (get-primes-sqrt) 1000000))
"Elapsed time: 123003.616566 msecs"
15485867
t.core=>
```

It's clearly more than linear, which is to be expected, but this is a huge
improvement.

### Functional Sieve

There's a completely different way of computing primes known as the Sieve of
Eratosthenes. The general concept is that, instead of looking at each number
and looking at its divisors, we can start from the list of all numbers and
remove from it all of the multiples of the first number in the list. The first
number of the remaining list is prime, and we can remove all of _its_
multiples.

```clojure
(defn get-primes-sieve-fn
  []
  (let [sieve (fn sieve [ls]
                (let [p (first ls)]
                  (cons p
                        (lazy-seq (sieve (remove #(zero? (rem % p))
                                                 (rest ls)))))))]
    (cons 2 (sieve (iterate #(+ 2 %) 3)))))
```

(As a minor optimization, we start knowing about 2 being prime, and then only
look at odd numbers starting at 3.)

This is a good way to illustrate the idea, but it doesn't work all that well in
practice. We're a lot faster than `get-primes`, because, for each number, we
stop as soon as we find _a_ divisor, rather than computing all of them. But all
those calls to `remove` wrap our lazy seq in an increasing number of closures,
which is not only slow, it's also building up the stack:

```clojure-repl
t.core=> (time (nth (get-primes-sieve-fn) 10))
"Elapsed time: 0.316836 msecs"
31
t.core=> (time (nth (get-primes-sieve-fn) 100))
"Elapsed time: 1.77067 msecs"
547
t.core=> (time (nth (get-primes-sieve-fn) 1000))
"Elapsed time: 119.300054 msecs"
7927
t.core=> (time (nth (get-primes-sieve-fn) 10000))
Execution error (StackOverflowError) at (REPL:1).
null

t.core=>
```

### Stateful Sieve

One of the nice properties of the Sieve approach is that, if one only wants to
compute primes up to a given upper bound, one can completely eschew division.
In Clojure terms:

```clojure
(defn sieve-upto
  [n]
  (loop [candidates (->> (cons 0 (cons 0 (rest integers)))
                         (take-while #(<= % n))
                         vec)
         start 2]
    (let [next-prime (get candidates start)]
      (case next-prime
        nil (vec (remove zero? candidates))
        0 (recur candidates (inc start))
        (recur (let [m (count candidates)]
                 (loop [i (+ start start)
                        c candidates]
                   (if (< i m)
                     (recur (+ i start)
                            (assoc c i 0))
                     c)))
               (inc start))))))
```

We start with a vector of integers up to our limit `n`, starting at 0, such
that the number at index `n` is `n`. We mark the first two elements of that
vector as 0. Then, for each position starting at 2, we look at the current
position:

- If we get `nil`, we're out of the vector.
- If we get 0, the current index is not prime and we move on to the next.
- Otherwise, we get the next prime. We leave it alone, but we mark all of its
  multiples as 0 in the vector of candidates.

We can look at how fast this is if we know what we're aiming for. For example,
we know from the above that the thousandth prime number is 7927 (starting the
count with 2 being the 0th):

```clojure-repl
t.core=> (time (last (sieve-upto 7930)))
"Elapsed time: 11.280216 msecs"
7927
t.core=>
```

But it would be easier if we could still have a list of primes, like the other
approaches gave us. We can do that by using the same `iterate`-based approach
introduced earlier. First, let's think about the state we need to carry over.
We'll need a function to generate a sieve up to a known number, and then we
need to:

- Have an existing list of prime numbers, so that's going to be part of our
  state.
- Know what prime index we're computing. If that's low enough to be in our
  current list, we just return that. In this case "return" means we need a
  piece of state that represents our return value.
- If the index is out of our current list, we need a bigger list. In order to
  produce that, we need to know what our last maximum was, increase it, and
  generate a new list of known primes.

This yields something like:

```clojure
(defn generate-sieve
  [sieve-fn]
  (fn [[idx bound primes prime]]
    (let [idx (inc idx)
          [primes bound] (if (< idx (count primes))
                           [primes bound]
                           (loop [bound (* 2 bound)]
                             (let [primes (sieve-upto bound)]
                               (if (> (count primes) idx)
                                 [primes bound]
                                 (recur (* 2 bound))))))
          prime (get primes idx)]
      [idx bound primes prime])))
```

> **EDIT 2021-11-28**: There is a mistake in the above function: it is
> hardcoding `sieve-upto` instead of using the passed-in `sieve-fn`. This
> invalidates all of the following discussion on performance of our two sieve
> implementations.

And we can now have a sieve-generated list of primes using our bounded sieve
calculation:

```clojure
(defn get-primes-sieve-vec
  []
  (->> [0 10 [2 3 5 7] 2]
       (iterate (generate-sieve sieve-upto))
       (map peek)))
```

and we can look at the performance of that:

```clojure-repl
t.core=> (time (nth (get-primes-sieve-vec) 100))
"Elapsed time: 1.490997 msecs"
547
t.core=> (time (nth (get-primes-sieve-vec) 1000))
"Elapsed time: 31.543777 msecs"
7927
t.core=> (time (nth (get-primes-sieve-vec) 10000))
"Elapsed time: 715.779816 msecs"
104743
t.core=> (time (nth (get-primes-sieve-vec) 100000))
"Elapsed time: 6698.746316 msecs"
1299721
t.core=> (time (nth (get-primes-sieve-vec) 1000000))
"Elapsed time: 147744.802956 msecs"
15485867
t.core=>
```

which is not quite as good as the `get-primes-sqrt` approach. But, of course, I
did not get through the trouble of defining the `generate-sieve` function
separately if I did not intend to show another way to compute the Sieve.

### Mutable Sieve

The Sieve function is nicely isolated, and it's easy to imagine how it's
currently very inefficient, with all those random "mutations" to an immutable
vector. In a real-world case we'd first confirm that with a profiler, but here
let's instead just try to replace that with a mutable array, and see what
happens.

The logic is essentially the same:

```clojure
(defn array-sieve
  [n]
  (let [a (long-array n)]
    (loop [idx 2]
      (when (< idx n)
        (aset-long a idx idx)
        (recur (inc idx))))
    (loop [idx 2]
      (when (< idx n)
        (let [v (aget a idx)]
          (when (not (zero? v))
            (loop [idx2 (* v v)]
              (when (< idx2 n)
                (aset-long a idx2 0)
                (recur (+ v idx2)))))))
      (when (< idx n)
        (recur (inc idx))))
    (vec (remove zero? a))))

(defn get-primes-sieve-arr
  []
  (->> [0 10 [2 3 5 7] 2]
       (iterate (generate-sieve array-sieve 2))
       (map peek)))
```

But the results don't really make sense:

```clojure-repl
t.core=> (time (nth (get-primes-sieve-arr) 100))
"Elapsed time: 1.159287 msecs"
547
t.core=> (time (nth (get-primes-sieve-arr) 1000))
"Elapsed time: 25.217017 msecs"
7927
t.core=> (time (nth (get-primes-sieve-arr) 10000))
"Elapsed time: 566.924858 msecs"
104743
t.core=> (time (nth (get-primes-sieve-arr) 100000))
"Elapsed time: 5817.740507 msecs"
1299721
t.core=> (time (nth (get-primes-sieve-arr) 1000000))
"Elapsed time: 124106.874773 msecs"
15485867
t.core=>
```

> **EDIT 2021-11-28**: Here are the numbers with a corrected `generate-sieve`:
>
> ```clojure-repl
> t.core=> (time (nth (get-primes-sieve-arr) 100))
> "Elapsed time: 0.580081 msecs"
> 547
> t.core=> (time (nth (get-primes-sieve-arr) 1000))
> "Elapsed time: 8.277411 msecs"
> 7927
> t.core=> (time (nth (get-primes-sieve-arr) 10000))
> "Elapsed time: 133.175033 msecs"
> 104743
> t.core=> (time (nth (get-primes-sieve-arr) 100000))
> "Elapsed time: 1102.844058 msecs"
> 1299721
> t.core=> (time (nth (get-primes-sieve-arr) 1000000))
> "Elapsed time: 20240.842127 msecs"
> 15485867
> t.core=>
> ```
>
> which make a lot more sense. It also looks like the difference between the
> numbers in the original post can be explained by JVM warmup:
>
> ```clojure-repl
> t.core=> (time (nth (get-primes-sieve-vec) 1000000))
> "Elapsed time: 133711.154576 msecs"
> 15485867
> t.core=> (time (nth (get-primes-sieve-vec) 1000000))
> "Elapsed time: 105065.534808 msecs"
> 15485867
> t.core=>
> ```

This is not as big a difference as I'd have expected. Let's look at just the
sieves:

```clojure-repl
t.core=> (time (last (sieve-upto 15485868)))
"Elapsed time: 43025.022763 msecs"
15485867
t.core=> (time (last (array-sieve 15485868)))
"Elapsed time: 7538.261028 msecs"
15485867
t.core=>
```

That's almost a 30s difference, on an operation we know must be happening. The
overall difference is closer to 23s, so something a little bit fishy may be
going on, though this could just be JVM warmup or something.

### No `iterate`

What's more annoying is where all the rest of the time is going. In order to
measure that, we could try to have a near-instant Sieve function:

```clojure-repl
t.core=> (def memo-sieve (memoize array-sieve))
#'t.core/memo-sieve
t.core=> (time (nth (->> [0 10 [2 3 5 7] 2]
                         (iterate (generate-sieve memo-sieve))
                         (map peek))
                    1000000))
"Elapsed time: 142113.8856 msecs"
15485867
t.core=> (time (nth (->> [0 10 [2 3 5 7] 2]
                         (iterate (generate-sieve memo-sieve))
                         (map peek))
                    1000000))
"Elapsed time: 111172.442033 msecs"
15485867
t.core=>
```

> **EDIT 2021-11-28**: The above is, again, completely bogus because of the bug
> in `generate-sieve`. With a corrected version, this looks like:
> ```clojure-repl
> t.core=> (time (nth (->> [0 10 [2 3 5 7] 2]
>                          (iterate (generate-sieve memo-sieve))
>                          (map peek))
>                1000000))
> "Elapsed time: 20214.719821 msecs"
> 15485867
> t.core=> (time (nth (->> [0 10 [2 3 5 7] 2]
>                                (iterate (generate-sieve memo-sieve))
>                                (map peek))
>                     1000000))
> "Elapsed time: 411.981361 msecs"
> 15485867
> t.core=>
> ```

So that `iterate` trick, while cute, seems to be taking up quite a bit of time.
Let's try condensing our computation a bit, and producing that list of primes
in a more direct way:

```clojure
(defn get-primes-final
  []
  (letfn [(h [p0 d bound]
            (concat (drop d p0)
                    (lazy-seq
                      (loop [bound (* 2 bound)]
                        (let [p1 (array-sieve bound)]
                          (if (> (count p1) (count p0))
                            (h p1 (count p0) bound)
                            (recur (* 2 bound))))))))]
    (h [2 3 5 7] 0 10)))
```

As the name might suggest, this is where we stop for this blog post. Let's see
how that performs:

```clojure-repl
t.core=> (time (nth (get-primes-final) 100))
"Elapsed time: 0.646813 msecs"
547
t.core=> (time (nth (get-primes-final) 1000))
"Elapsed time: 8.850757 msecs"
7927
t.core=> (time (nth (get-primes-final) 10000))
"Elapsed time: 143.092929 msecs"
104743
t.core=> (time (nth (get-primes-final) 100000))
"Elapsed time: 1268.277835 msecs"
1299721
t.core=> (time (nth (get-primes-final) 1000000))
"Elapsed time: 20626.98392 msecs"
15485867
t.core=> (time (nth (get-primes-final) 10000000))
Execution error (OutOfMemoryError) at t.core/array-sieve (core.clj:98).
Java heap space

t.core=>
```

I suppose I may have grown a bit too ambitious towards the end there.

### Conclusion

As mentioned in the introduction, this is not about prime numbers. There's no
practical application to computing a list of prime numbers. There are
applications for checking primality, and for generating very large primes, but
I'm not aware of any for "a lazy list of all primes".

This post is mostly about me sharing a toy problem I've played with. If you've
reached this point, I hope you found the above interesting, but more
importantly, I encourage you to, like I did, pick out a small, simple, abstract
problem and play with it in order to gain a deeper understanding of a
programming language.
