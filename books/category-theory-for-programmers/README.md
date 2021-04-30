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
```

Note that _identity_ here is to be understood in the context of morphisms: the
identity morphism is the one that leaves a morphism unchanged through
composition. It is **not** the function that leaves its argument unchanged.

```clojure
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

### Chapter 5 - Products and Coproducts

Because category theory only deals with opaque objects and labeled but still
opaque morphisms, the only thing we can really comment on is the shapes, or
patterns, of morphisms and objects.

The simplest such pattern is called the _initial object_ and is an object that
has exactly one morphism to every other object in the categiry. Not all
categories have an initial object. There may be more than one such object, but
in that case they are all _isomorphic_ to each other. Therefore, we say that
when there is an initial object, it is _unique up to isomorphisms_.

In the category defined by less-than-or-equal over the naturals, 0 would be an
initial object. In a category defined by less-than-or-equal over the integers,
there is no initial object. In Set, the initial object is the empty set.

The reciproqual pattern is called the _terminal object_: an object such that
there is a single morphism to it from every other object. Here again not every
category has a terminal object, and some categories may have more than one, but
if so they are all isomorphic so the terminal object is unique up to
isomorphisms.

In a partially ordered set, the terminal object would be the biggest object. In
Set, the terminal object is the singleton set (`()` in Haskell).

For any category $C$, one can define a _dual_ category $C^{op}$ by keeping all
the objects but reversing all the arrows, and redefining composition to also
reverse it. Constructions in the dual category are usually called co-something.

An isomorphism is a pair of morphisms such that their composition is the
identity (on both ends). Because of composition, in a proper category this
means that they have the same shape (same set of incoming/outgoing arrows).

A _product_ of two objects $a$ and $b$ is an object $c$ such that there is a
morphism $f$ from $c$ to $a$, and a morphism $g$ from $c$ to $b$, and there is
no object $c'$ such that there is a morphism $f'$ from $c'$ to $a$ and a
morphism $g'$ from $c'$ to $b$ such that there exists a morphism $m$ from $c$
to $c'$ such that $f = f' \dot m$ and $g = g' \dot m$.

A _coproduct_ is the dual of a product: $c$ is a coproduct of $a$ and $b$ if
there exist two morphisms $f$ from $a$ to $c$ and $g$ from $b$ to $c$ such that
there is no $c'$ with $f'$ from $c'$ to $a$ and $g'$ from $c'$ to $b$ such that
$f = m \dot f'$ and $g = m \dot g'$, where $m$ goes from $c$ to $c'$.

In Set, products are pairs and coproducts are (tagged/disjoint) unions.

Because functions are, in general, not invertible, there is an asymmetry
between products and coproducts in Set.

A function whose image is its entire codomain is called _surjective_; a
function that is defined on its whole domain is called _injective_; a function
that is both at the same time is called _bijective_ (and is invertible), or,
from a category perspective, an isomorphism. Note that it does mean two
isomorphic sets have to have the same size.

#### Challenges

> 1. Show that the terminal object is unique up to unique isomorphism.

Let $t_1$, $t_2$ be two terminal objects. By definition, a terminal object has
a single incoming morphism from every object, which means that there can be
only one morphism from the object to itself and that morphism is the identity.
Because $t_1$ is terminal, there must be a morphism from $t_2$ to $t_1$, and
because $t_2$ is terminal, there must be a morphism from $t_1$ to $t_2$. If we
assume that we are in a category, the composition of these two morphisms must
be the identities for $t_1$ and $t_2$ and therefore these two morphisms form an
isomorphism. Furthermore, as these two morphisms are the only ones between
these two object, the isomorphism itself is unique.

> 2. What is a product of two objects in a poset? Hint: Use the universal
>    construction.

If there is a direct morphism between the two object, the minimum of the two.
Otherwise, if such an object exists, it is the largest object that is smaller
than both of the objects. If no such object exists (disjoint DAG), there is no
product for these two objects in this poset.

> 3. What is a coproduct of two objects in a poset?

Maximum of the two objects, modulo same cases as product (smallest one that's
largest than both etc.).

> 4. Implement the equivalent of Haskell `Either` as a generic type in your
>    favorite language (other than Haskell).

```clojure
(defn left [v] [:left v])
(defn right [v] [:right v])
```

> 5. Show that `Either` is a "better" coproduct than `int` equipped with two
>    injections:
>    ```
>    int i(int n) { return n; }
>    int j(bool b) { return b ? 0 : 1; }
>    ```
>    Hint: Define a function
>    ```
>    int m(Either const & e);
>    ```
>    that factorizes `i` and `j`.

I don't actually know enough C++ to implement that, but in Haskell:

```haskell
m :: Either Int Bool -> Int
m Left i = i
m Right True = 0
m Right False = 1
```

So `Either` is better because it can do everything `i` and `j` can do.

> 6. Continuing the previous problem: how would you argue that `int` with the
>    two injections `i` and `j` cannot be "better" than `Either`?

The problem here is that the conversion is lossy: we don't know if we started
with `false` or `1` (or `true` vs. `0`), whereas `Either` registers that
information. In other words the union is not tagged.

> 7. Still continuing: what about these injections?
>    ```
>    int i(int n) {
>        if (n < 0) return n;
>        return (n + 2);
>    }
>
>    int j(bool b) { return b ? 0 : 1; }
>    ```

If `int` were real (pun?) integers, that would work, but as it stands it's
still lossy because of the overflow issue: we're not mapping the entire domain.

> 8. Come up with an inferior candidate for a coproduct of `int` and `bool`
>    that cannot be better than `Either` because it allows multiple acceptable
>    morphisms from it to `Either`.

```haskell
data Either2 a b b = Either2 a b b
```

### Chapter 6 - Simple Algebraic Data Types

Because pairs `(a, b)` and `(b, a)` are isomorphic (`swap` function), nested
pairs can be represented as tuples. Products (of sets) look like
multiplication, with the singleton set playing the role of `1` (which is why
it's called `Unit`).

Set is a _monoidal category_, i.e. a category that is also a monoid as you can
define a product between any two of its objects.

It is also a mpnoid with respect to addition of types with `Void` playing the
role of `0`. In fact, because addition and multiplication of types compose, Set
forms a _semiring_, i.e. types have the same properties as natural numbers over
addition and multiplication (a full _ring_ would require support for
subtraction). Booleans with `||` and `&&` also form a semiring, which will be
important later on.

> The rest of the chapter explains `data` declarations: named constructors,
> types with multiple type variables, records. This is Haskell syntax and not
> category theory.

#### Challenges

> 1. Show the isomorphism between `Maybe a` and `Either () a`.

```haskell
m_to_e :: Maybe a -> Either () a
m_to_e Nothing = Left ()
m_to_e (Just a) = Right a

e_to_m :: Either () a -> Maybe a
e_to_m (Left _) = Nothing
e_to_m (Right a) = Just a
```

> 2. Here's a sum type defined in Haskell:
>    ```
>    data Shape = Circle Float | Rect Float Float
>    ```
>    When we want to define a function like `area` that acts on a `Shape`, we
>    do it by pattern matching on the two constructors:
>    ```
>    area :: Shape -> Float
>    area (Circle r) = pi * r * r
>    area (Rect d h) = d * h
>    ```
>    Implement `Shape` in C++ or Java as an interface and create two classes
>    `Circle` and `Rect`. Implement `area` as a virtual function.

I choose Java.

```java
public interface Shape {
  double area();

  static class Circle implements Shape {
    public final double r;
    public Circle(double r) {
      this.r = r;
    }
    public double area() {
      return Math.PI * r * r;
    }
    // equal & hashcode left as an exercise
  }

  static class Rect implements Shape {
    public final double h;
    public final double d;
    public Rect(double d, double h) {
      this.d = d;
      this.h = h;
    }
    public double area() {
      return d * h;
    }
    // equal & hashcode left as an exercise
  }
}
```

> 3. Continuing with the previous example: We can easily add a new function
>    `circ` that calculates the circumference of a `Shape`. We can do it
>    without touching the definition of `Shape`:
>    ```
>    circ :: Shape -> Float
>    circ (Circle r) = 2.0 * pi * r
>    circ (Rect d h) = 2.0 * (d + h)
>    ```
>    Add `circ` to your C++ or Java implementation. What parts of the original
>    code did you have to touch?

None:

```java
public interface ShapeUtils {
  static circ(Shape s) {
    if (s instanceof Shape.Rect) {
      Shape.Rect r = (Shape.Rect) s;
      return 2 * (r.d + r.h);
    } else if (s instanceof Shape.Circle) {
      Shape.Circle c = (Shape.Circle) s;
      return 2 * c.r * Math.PI;
    } else {
      throw new UnsupportedOperationException();
    }
  }
}
```

> 4. Continuing further: Add a new shape, Square, to Shape and make all the
>    necessary updates. What code did you have to touch in Haskell vs. C++ or
>    Java? (Even if you’re not a Haskell programmer, the modifications should
>    be pretty obvious.)

Let's see. In Haskell, pretty much all of it: we need to change the type _and_
both functions.

```haskell
data Shape = Circle c | Rect d h | Square s

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rect d h) = d * h
area (Square s) = s * s

circ :: Shape -> Float
circ (Circle r) = 2.0 * pi * r
circ (Rect d h) = 2 * (d + h)
circ (Square s) = 4 * s
```

In Java, none of it, we can just add some new code:

```java
public class Square extends Shape.Rect {
  public Square(double c) {
    super(c, c);
  }
}
```

> 5. Show that $a + a = 2 * a$ holds for types (up to isomorphism). Remember that
>    2 corresponds to Bool, according to our translation table.

Using Haskell notation, `a + a` is `Either a a`, whereas `2 * a` is `(Bool,
a)`. We can make an isomorphism with the two following functions:

```haskell
s_to_p :: Either a a -> (Bool, a)
s_to_p (Right a) = (True, a)
s_to_p (Left a) = (False, a)

p_to_s :: (Bool, a) -> Either a a
p_to_s (True, a) = Right a
p_to_s (False, a) = Left a
```

### Chapter 7 - Functor

A functor is a mapping between categories that maps every morphism $f \dblcolon
a \to b$ in the source category to $Ff \dblcolon Fa \to Fb$ in the target
category, every composition $g \cdot f$ to the composition $Fg \cdot Ff$, and
every identity morphism $\textrm{id}_{a}$ such that $F\textrm{id}_{a} =
\textrm{id}_{Fa}$.

According to these rules, functors can collapse and embed, but not tear apart.
The particular functor that reduces everything to a single object and morphism
(the identity for that object in the target category) is caled $\Delta_c$.

An _endofunctor_ is a functor from one category to itself. A common example in
programming languages are _type constructors_ (or parametric types) such as
`Maybe`, which can be endofunctors in Set (or Hask).

Functors have to transform morphisms, so for a type constructor to "be" a
functor, it needs to also be applicable to functions. In the case of `Maybe`,
this can be done by defining:

```haskell
fmap :: (a -> b) -> (Maybe a -> Maybe b)
fmap f = \case
  Nothing -> Nothing
  Just a -> Just (f a)
```

In the same way, we can make the `List` type constructor a functor by defining:

```haskell
fmap :: (a -> b) -> [a] -> [b]
fmap f = \case
  [] -> []
  (a:as) -> f a : fmap f as
```

The type constructor `(->) r` can also be made a functor by defining:

```haskell
fmap :: (a -> b) -> ((->) r) a -> ((->) r) b
fmap f g = f . g
```

The type constructor `(Const c)` can also be made a functor by defining:

```haskell
fmap :: (a -> b) -> Const c a -> Const c b
fmap _ const = const
```

This is a special-case (specifically, the "endofunctor in Hask" case) of
$\Delta_c$.

Functors can be seen as morphisms in a category where objects are categories.
The category of all "small" categories is called Cat. (It doesn't include
itself, because Cat is big.)

#### Challenges

> 1. Can we turn the Maybe type constructor into a functor by defining:
> ```haskell
> fmap _ _ = Nothing
> ```
> which ignores both of its arguments? (Hint: Check the functor laws.)

No: identity is not preserved. With that definition, `fmap id (Just x)` is
`Nothing` instead of `id (Just x)`.

> 2. Prove functor laws for the reader functor. Hint: it's really simple.

Identity:

```haskell
fmap id f = id . f = f
id f = f
```

Composition:

```haskell
fmap (f . g) h = (f . g) . h
               = f . g . h
(fmap f . fmap g) h = (fmap f) (fmap g h)
                    = f . (g . h)
                    = f . g . h
```

> 3. Implement the reader functor in your second favorite language (the first
>    being Haskell, of course).

```java
import java.util.function.Function;

public final class Reader<R> {
  private Reader() {}
  public static <A, B> Function<Function<R, A>, Function<R, B>> fmap(Function<A, B> f) {
    return g -> r -> f.apply(g.apply(r));
  }
}
```

> 4. Prove the functor laws for the list functor. Assume that the laws are true
>    for the tail part of the list you’re applying it to (in other words, use
>    induction).

For reference:

```haskell
fmap f = \case
  [] -> []
  (a:as) -> f a : fmap f as
```

Identity:

```haskell
fmap id [] = []
id [] = []
fmap id (a:as) = id a : fmap id as
               = a : id as
               = a : as
               = id (a:as)
```

Composition:

```haskell
fmap (f . g) [] = []
(fmap f . fmap g) [] = fmap f (fmap g []) = fmap f [] = []

fmap (f . g) (a:as)
  = (f . g) a : fmap (f . g) as
(fmap f . fmap g) (a:as)
  = fmap f (fmap g (a:as))
  = fmap f (g a : fmap g as)
  = f (g a) : fmap f (fmap g as)
  = (f . g) a : (fmap f . fmap g) as
```

### Chapter 8 - Functoriality

A _bifunctor_ is a functor of two arguments, i.e. from C x D to E. We define
composition in C x D as (f, g) . (f', g') = (f . f', g . g'). In Haskell, a
(endo?) bifunctor (Hask x Hask -> Hask) can be defined with:

```haskell
class Bifunctor f where
  bimap :: (a -> c) -> (b -> d) -> f a b -> f c d
  bimap g h = first g . second h
  first :: (a -> c) -> f a b -> f c b
  first g = bimap g id
  second :: (b -> d) -> f a b -> f a d
  second = bimap id
```

Both `Either` and `(,)` have a natural `Bifunctor` instance. Since `Const ()`
and `Identity` are also functors, any algebraic data type is functorial by
construction. In Haskell, this construction (for bifunctors) can be written as:

```haskell
newtype BiComp bf fu gu a b = BiComp (bf (fu a) (gu b))
instance (Bifunctor bf, Functor fu, Functor gu) => Bifunctor (BiComp bf fu gu)
where
  bimap f1 f2 (BiComp x) = BiComp ((bimap (fmap f1) (fmap f2)) x)
```

For example, this means that `Maybe` is a functor because it is built out of
`Const ()` (= `Nothing`) and `Identity` (= `Just`) using `Either`.

The `DeriveFunctor` Haskell extension lets the compiler automatically derive
`fmap` implementations using `deriving Functor` on any algebraic data type.

In a Kleisli category, we have `>=>` and `return` with the signatures:

```haksell
(>=>) :: (a -> k b) -> (b -> k c) -> (a -> k c)
return :: a -> k a
```

which means we can define `fmap` (and thus make up a functor) with:
```haskell
fmap f = id >=> (\x -> return (f x))
```

All of those functors are called _covariant_ functors. There are also
_contravariant_ functors, which are mappings that reverse morphisms (but apart
from that still respect composition). In Haskell, they correspond to the class:

``` haskell
class Contravariant f where
  contramap :: (b -> a) -> (f a -> f b)
```

The type constructor `(-> r)` is a contravariant functor with `contramap = flip
(.)` (as opposed to `(a ->)` which is covariant with `fmap = .`).

In Haskell, a type with two parameters such that one is covariant and the other
is contravariant (like `->`) is called a `Profunctor` and defined by:

```haskell
class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
  dimap f g = lmap f . rmap g
  lmap :: (a -> b) -> p b c -> p a c
  lmap f = dimap f id
  rmap :: (b -> c) -> p a b -> p a c
  rmap = dimap id
```

and thus

```haskell
instance Profunctor (->) where
  lmap = flip (.)
  rmap = (.)
  dimap ab cd bc = cd . bc . ab
```

#### Challenges

> 1. Show that the data type:
>    ```haskell
>    data Pair a b = Pair a b
>    ```
>    is a bifunctor. For additional credit implement all three methods of
>    Bifunctor and use equational reasoning to show that these definitions are
>    compatible with the default implementations whenever they can be applied.

```haskell
instance Bifunctor Pair where
  bimap f g (Pair a b) = Pair (f a) (g b)
  first f (Pair a b) = Pair (f a) b
  second g (Pair a b) = Pair a (g b)
```

> 2. Show the isomorphism between the standard definition of `Maybe` and this
>    desugaring:
>    ```haskell
>    type Maybe' a = Either (Const () a) (Identity a)
>    ```
>    Hint: Define two mappings between the two implementations. For additional
>    credit, show that they are the inverse of each other using equational
>    reasoning.

```haskell
pt :: Maybe' a -> Maybe a
pt (Left _) = Nothing
pt (Right (Identity a)) = Just a

tp :: Maybe a -> Maybe' a
rp Nothing = Left (Const ())
rp (Just a) = Right (Identity a)
```

> 3. Let’s try another data structure. I call it a `PreList` because it’s a
>    precursor to a `List`. It replaces recursion with a type parameter `b`.
>    ```haskell
>    data PreList a b = Nil | Cons a b
>    ```
>    You could recover our earlier definition of a `List` by recursively applying
>    `PreList` to itself (we’ll see how it’s done when we talk about fixed
>    points).
>    Show that `PreList` is an instance of `Bifunctor`.

```haskell
instance Bifunctor PreList where
  bimap f g Nil = Nil
  bimap f g (Cons a b) = Cons (f a) (g b)
  first f Nil = Nil
  first f (Cons a b) = Cons (f a) b
  second g Nil = Nil
  second g (Cons a b) = Cons a (g b)
```

> 4. Show that the following data types define bifunctors in `a` and `b`:
>    ```haskell
>    data K2 c a b = K2 c
>    data Fst a b = Fst a
>    data Snd a b = Snd b
>    ```
>    For additional credit, check your solutions against Conor McBride’s paper
>    [Clowns to the Left of me, Jokers to the Right][clowns].
>    [clowns]: http://strictlypositive.org/CJ.pdf

```haskell
instance Bifunctor (K2 c) where
  bimap _ _ (K2 c a b) = c
  first _ (K2 c a b) = c
  second _ (K2 c a b) = c
instance Bifunctor Fst where
  bimap f _ (Fst a) = Fst (f a)
  first f (Fst a) = Fst (f a)
  second _ f = f
instance Bifunctor Snd where
  bimap _ g (Snd b) = Snd (g b)
  first _ s = s
  second g (Snd b) = Snd (g b)
```

> 5. Define a `bifunctor` in a language other than Haskell. Implement `bimap`
>    for a generic pair in that language.

```clojure
(defprotocol Bifunctor
  (bimap [this f g])
  (first [this f])
  (second [this g]))
(extend-protocol Bifunctor
  clojure.lang.PersistentVector
  (bimap [[a b] f g] [(f a) (g b)])
  (first [[a b] f] [(f a) b])
  (second [[a b] g] [a (g b)]))
```

> 6. Should `std::map` be considered a bifunctor or a profunctor in the two
>    template arguments `Key` and `T`? How would you redesign this data type to
>    make it so?

I don't know C++. Changing keys to a map seems dangerous, but maybe a simple
list of pairs (allowing repetition of keys) could be made into a bifunctor.

### Chapter 9 - Function Types
