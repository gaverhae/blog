{:title "Computing prime numbers with Clojure"
 :layout :post
 :tags ["clojure"]}

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

```clojure
(defn nth-prime
  [n]
  (->> primes
       (drop n)
       first))
```

### Performance

The above definitions are correct from a mathematical perspective, and seem to
work at first:

```clojure-repl
t.core=> (time (nth-prime 0))
"Elapsed time: 0.344016 msecs"
2
t.core=> (time (nth-prime 10))
"Elapsed time: 0.943427 msecs"
31
t.core=> (time (nth-prime 100))
"Elapsed time: 56.789246 msecs"
547
t.core=>
```

But that last one seems a bit slow, doesn't it? Let's push a bit further.

```clojure-repl
t.core=> (time (nth-prime 1000))
"Elapsed time: 7613.744297 msecs"
7927
t.core=>
```

That looks like we're taking about one second _per integer_ we filter through.
That's definitely not good.

However:

```clojure-repl
t.core=> (time (nth-prime 1000))
"Elapsed time: 7613.013248 msecs"
7927
t.core=> (time (nth-prime 1000))
"Elapsed time: 0.201189 msecs"
7927
t.core=>
```


