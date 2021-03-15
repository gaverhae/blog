# Category Theory for Programmers

> Category Theory for Programmers, by Bartosz Milewski.
> - [Online book](https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/)
> - [PDF](https://s3.amazonaws.com/milewski-ctfp-pdf/category-theory-for-programmers.pdf) ([build](https://github.com/hmemcpy/milewski-ctfp-pdf/))
> - [Videos](https://www.youtube.com/playlist?list=PLbgaMIhjbmEnaH_LTkxLI7FMa2HsnawM_)

### Introduction

Category theory is about decomposing problems in sub-problems.

### Chapter 1 - Category: The Essence of Composition

A category is a tuple $<O, M>$ such that:
- $M \subseteq O \times O$
- $\forall o \in O: (o, o) \in M$
- $\forall (a, b), (b, c) \in M: (a, c) \in M$

The elements of $O$ are called _objects_ and the elements of $M$ are called
_morphisms_. A morphism of the form $(o, o)$ is called _identity_, and the last
property above is called _composition_.

#### Challenges

> 1. Implement, as best as you can, the identity function in your favorite
>    language (or the second favorite, if your favorite language happens to be
>    Haskell).

```clojure
(defn id [x] x)
```

> 2. Implement the composition function in your favorite language. It takes two
>    functions as arguments and returns a function that is their composition.

```clojure
(defn comp
  [f g]
  (fn [x & args]
    (apply f (g x) args)))
```

> 3. Write a program that tries to test that your composition function respects
>    identity.

Let's "prove" it instead:

```clojure
(comp id f) == (fn [x & args] (apply id (f x) args))
             ; definition of comp
            == (fn [x] (apply id (f x)))
             ; id only takes one argument
            == (fn [x] (id (f x)))
             ; definition of apply
            == (fn [x] (f x))
             ; definition of id
            == f
             ; definition of fn
(comp f id) == (fn [x & args] (apply f (id x) args))
             ; definition of comp
            == (fn [x & args] (apply f x args))
             ; definition of id
            == (fn [& args] (apply f args))
             ; definition of (apply x args)
            == f
             ; definitiong of (apply args)
```

> 4. Is the world-wide web a category in any sense? Are links morphisms?

The web is typically defined as the set of all "web pages" and the links
between them. In that sense, the web does not form a category:

1. Not all pages have self-link, hence not all object has an identity morphism.
2. Pages usually do not have all of their transitive links, so composition is
   not respected.

> 5. Is Facebook a category, with people as objects and friendships as
>    morphisms?

Same as above: people are generally not friends with themselves and friendship
is not transitive.

> 6. When is a directed graph a category?

When it respects the defintion given above. Examples:
- A complete graph is a category: $M = O \times O$.
- A graph with only self-links is a category: $M = { (o, o) \forall o \in O }$.
- The graph defined by $M = { (a, a), (b, b), (c, c), (a, b), (b, c), (a, c) }$.

### Chapter 2 - Types and Functions

A type can be seen as a set of values, except that programming language
functions, unlike mathematical functions, have to _compute_ a value and thus
may _non-terminate_. This is represented by an implicit additional value
_bottom_ in each type. The category where sets are objects and functions are
morphisms is called Set; the related but different category where each set has
been enriched with bottom is called Hask. In this book, we ignore the
difference between them and instead pretend that Set accurately represents the
Haskell type system. (This [can be justified][fast-and-loose].)

[fast-and-loose]: http://www.cs.ox.ac.uk/jeremy.gibbons/publications/fast+loose.pdf

The set with no value is called Void; there is one function from Void to any
other type, though it can never be called. As there is no value in the type
Void, there can be no function that returns Void and thus no morphism pointing
to Void.

The set with one value is called Unit. There is one function from any
other set to Unit. For any set of size N, there are exactly N functions from
Unit to that set (including Unit).

The set with two elements is usually called Bool.

#### Challenges

> 1. Define a higher-order function (or a function object) memoize in your favorite language.

```clojure
(defn memoize
  [f]
  (let [store (atom {})]
    (fn [& args]
      (if-let [resp (@store args)]
        resp
        (let [resp (apply f args)]
          (swap! store assoc args resp)
          resp)))))
```

> 2. Try to memoize a function from your standard library that you normally use
>    to produce random numbers. Does it work?

No.

> 3. Most random number generators can be initialized with a seed. Implement a
>    function that takes a seed, calls the random number generator with that
>    seed, and returns the result. Memoize that function. Does it work?

No.

> 4. Which of these C++ functions are pure? Try to memoize them and observe
>    what happens when you call them multiple times: memoized and not.

Well, my favourite language is not C++, so I don't have a memoizer handy. But I
can answer anyway.

> (a) The factorial function from the example in the text.

Yes.

> (b) std::getchar()

No.

> (c) `bool f() { std::cout << "Hello!" << std::endl; return true; }`

No.

> (d) `int f(int x) { static int y = 0; y += x; return y; }`

No.

> 5. How many different functions are there from Bool to Bool? Can you
>    implement them all?

Four:

```clojure
(defn always-true [_] true)
(defn always-false [_] false)
(defn the-same [b] b)
(defn the-other [b] (not b))
```

> 6. Draw a picture of a category whose only objects are the types Void, ()
>    (unit), and Bool; with arrows corresponding to all possible functions
>    between these types. Label the arrows with the names of the functions.

Not exactly the right medium here. Let's list outgoing arrows:

- From Void, there is one arrow to Unit and one to Bool.
- From Unit, there is one arrow to itself, and two arrows to Bool.
- From Bool, there is one arrow to Void, one to Unit, and four to itself.

### Chapter 3 - Categories Great and Small

The empty category makes as much sense as the empty set. A category created by
taking a random graph and adding composition arrows is called a free category.

A category where a morphism is a "smaller than (or equal to)" relationship is
called an _order_. A set with such a relation is called a _preorder_; with the
additional constraint that $a \leq b \and b \leq a \to a = b$, we call it a
_partial order_. If there is such a relation between any two objects, then we
have a _linear_ or _total_ order.

A category where there is _at most_ one morphism between any two objects (e.g.
a preorder) is called _thin_. The set of morphisms between two objects in a
category is called the hom-set of those two objects and is written $C(a,b)$ or
$\textrm{Hom}\_C(a,b)$.

Sorting only works in categories with a total order.

A monoid is a set and a binary operation such that:

- The binary operation is associative for elements of that set, and
- There is an element of the set that acts as a zero for the operation.

In category terms, a monoid is a category with a single object and many arrows
from that object to itself.

A category in which morphisms between any two objects form a set is called
locally small.

#### Challenges

> 1. Generate a free category from:
>   a. A graph with one node and no edges.
>   b. A graph with one node and one (directed) edge (hint: this edge can be composed with itself).
>   c. A graph with two nodes and a single arrow between them.
>   d. A graph with a single node and 26 arrows marked with the letters of the alphabet (a-z).

a. Add an identity.
b. Kind of depends on where the arow ends. If it ends on the node, it's already
   a category. If it is dangling, add a node at the end of the arrow and two
   identities.
c. Add identities.
d. Add an identity and loads of compositions.

> 2. What kind of order is this?
>  a. A set of sets with the inclusion relation: A is included in B if every
>     element of A is also an element of B.
>  b. C++ types with the following subtyping relation: T1 is a subtype of T2 if a
>     pointer to T1 can be passed to a function that expects a pointer to T2
>     without triggering a compilation error.

a. Partial order (A has all the elements of A).
b. Partial order.

> 3. Considering that Bool is a set of two values True and False, show that it
>    forms two (set-theoretical) monoids with respect to, respectively,
>    operator && (AND) and || (OR).

Both operations are associative; True is a zero for && and False is a zero for
||.

> 4. Represent the Bool monoid with the AND operator as a category: List the
>    morphisms and their rules of composition.

There is only one object, and both morphisms go from it to it, so all we need
to define are the labels of the morphisms:

a. (&& True) is identity.
b. (&& False) composes with itseld as itself.

5. Represent addition modulo 3 as a monoid category.

a. (+0) is identity.
b. (+1) composed with (+1) gives (+2); composed with (+2) gives identity.
c. (+2) composed with (+1) is identity; composed with itself is (+1).

### Chapter 4 - Kleisli Categories

We start with the Set category, but replace all the functions with functions
that return a pair of (result, output), so we can concatenate all the outputs.
This simulates logging, but pure. This means we need a new composition rule:
call the second function on the result only, and then concatenate both outputs.
Identity functions have to return no output (empty string). Note that the log
could be any monoid.

```haskell
id x = (id, mzero)
comp f g x =
  let (v1, out1) = f x
      (v2, out2) = g v1
  in (v2, mappend out1 out2)
```

This is known as a Kleisli category, and it is built around a monad, which
we'll explain much later.

#### Challenges

> 1. Construct the Kleisli category for partial functions (define composition
>    and identity).

```clojure
(defn identity
  [x]
  [x])

(defn composition
  [f g]
  (fn [x]
    (let [a (f x)]
      (if (empty? a) [] (g (first a))))))
```

> 2. Implement the embellished function safe_reciprocal that returns a valid
>    reciprocal of its argument, if it’s different from zero.

```clojure
(defn safe-reciprocal
  [x]
  (if (zero? x)
    []
    [(/ 1.0 x)]))
```

> 3. Compose the functions safe_root and safe_reciprocal to implement
>    safe_root_reciprocal that calculates sqrt(1/x) whenever possible.

```clojure
(defn safe-root
  [x]
  (if (> x 0) [(Math/sqrt x)] []))

(def safe-root-reciprocal (cmoposition safe-root safe-reciprocal))
```
