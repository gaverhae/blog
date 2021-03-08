# Category Theory for Programmers

> Category Theory for Programmers, by Bartosz Milewski.
> - [Online book](https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/)
> - [PDF](https://s3.amazonaws.com/milewski-ctfp-pdf/category-theory-for-programmers.pdf) ([build](https://github.com/hmemcpy/milewski-ctfp-pdf/))
> - [Videos](https://www.youtube.com/playlist?list=PLbgaMIhjbmEnaH_LTkxLI7FMa2HsnawM_)

### Introduction

Category theory is about decomposing problems in sub-problems.

### Chapter 1

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
