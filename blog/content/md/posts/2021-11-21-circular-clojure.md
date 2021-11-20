{:title "Circular Programs in Clojure"
 :layout :post
 :tags ["clojure" "papers"]}

A couple years ago, I decided to learn Haskell. This led me to develop an
interest for [lambda calculus], [interpreters], and [laziness]. (And monads,
[in general][monads] as well as [in Clojure][monad-clj], but that is less
relevant for today's topic.) While researching those topics, I came across a
paper meshing all those topics together by explaining a nice technique to
implement embedded functional languages in Haskell. On the face of it, the
technique presented seems to rely so heavily on Haskell language features —
primarily whole-language laziness — that it looks like it would be hard to
implement in any other language.

This, of course, looked like a challenge to me, and I immediately started
wondering how hard it would be to implement the same technique in Clojure. Then
I got distracted and left that problem alone for a few months. That is, until I
got a bit of an [epiphany] while playing with [prime numbers] two weeks ago.

But first, let's take a look at [the paper itself][paper].

### The Problem

The paper is entitled "[Using Circular Programs for Higher-Order
Syntax][paper]"; as may be expected, the problem it tries to solve is
representing "higher-order syntax", and the solution the authors came up with
relies on "circular programs". Those terms may not be immediately familiar, so
let's break that down.

At the core of the problem is [lambda calculus]. Without going into all of the
details, the idea here is to simplify the notion of a programming language to
the extreme, an specifically to the point where the language is composed of
only three forms: variables, single-argument function definitions, and function
application.

It turns out these three forms are enough to form a complete model of
computation, but that's not quite what I want to focus on here. For our
purposes today, it is enough that:

- We agree any reasonable "functional language" we may want to implement as an
  embedded language will have functions.
- Higher-order syntax is a nice API to define programs in that language (more
  on that later).
- It turns out there is a bit of a thorny issue to solve there, and this
  minimal language is enough to illustrate it.

As we'll want to write some sort of interpreter for our embedded language, we
should define an [abstract syntax] for it. Let's go for:

```clojure
;; a variable, identified by a number
[:var 1]
;; a function definition (identity)
[:abs 2 [:var 2]]
;; a function application applying identity to a free variable
[:app [:abs 1 [:var 1]] [:var 2]]
```

Note that, semantically, that last line has the same meaning as:

```clojure
[:app [:abs 2 [:var 2]] [:var 2]]
```

because of lexical scope, but now, as a human reader, we have to realize that
there are two _different_ variables named `2` at play. Also note that we could
have chosen to use strings for variable names instead of numbers, and probably
would in a real context, but for our purposes here all we need is equality
checks and numbers are the simplest way to get that.

If we want to write more complex expressions, it can become a bit tedious. For
example, here is [the `+` operator in Church encoding][lambda]:

```clojure
[:abs 0
 [:abs 1
  [:abs 2
   [:abs 3
    [:app [:app [:var 0] [:var 2]]
          [:app [:app [:var 1] [:var 2]]
                [:var 3]]]]]]]
```

Now, part of the tediousness comes from the language itself: by defining such a
small core language, we inevitably get some verbosity in defining useful
values.

But part of the tediousness also comes from the use of a data representation of
what should naturally be functions. Moreover, there is an inherent opportunity
for human error in writing down those expressions, as it is easy to mistype a
variable name.

To address those issues, we can use "higher-order syntax", which is a fancy way
of saying let's use our host language functions to represent our embedded
language functions. This way, we get our host language semantics for free when
it comes to checking our argument names (and possibly types) and dealing with
shadowing.

This means defining two functions, `abs` and `app`, such that the above can be
written:

```clojure
(abs (fn [m]
  (abs (fn [n]
    (abs (fn [f]
      (abs (fn [x]
        (app (app m f)
             (app (app n f)
                  x))))))))))
```

which is still as complicated in terms of the logic of this function (because
Church encoding is complicated), but is syntactically much nicer, and arguably
less error-prone even in a language with no static type-checking.

And so the problem the paper sets out to solve is: can we define `abs` and
`app` with the presented API and reasonable performance?

Specifically, the paper uses Haskell, so, for clarity, these are the types in
play:

```haskell
data Exp = Var Int      --  ex: [:var 1]
         | Abs Int Exp  --  ex: [:abs 2 [:var 2]]
         | App Exp Exp  --  ex: [:app [:abs 2 [:var 2]] [:var 2]]

app :: Exp -> Exp -> Exp
abs :: (Exp -> Exp) -> Exp
```

If you're not familiar with Haskell, those last two lines mean that `app` is a
function of two `Exp` arguments and returns another `Exp` argument, while `abs`
is a function that takes a single argument which is itself a function from
`Exp` to `Exp`, and returns an `Exp`.

At this point, it's important to realize that we're not trying to _calculate_
anything: `app` and `abs` are just meant as syntactic sugar to _construct_
`Exp` values.

In other words, we could expect `app` to just be `(fn [e1 e2] [:app e1 e2])`.
The implementation of `abs` is less obvious, as it needs to introduce names for
the `Abs` arguments, and that's the problem the paper tackles.

### Why `abs` is non-trivial

The paper remains a bit more abstract in the representation of variable names,
and keeps them as "any sufficiently large, totally ordered set"; in this blog,
I'll just represent them as integers. Any other countable (or smaller), ordered
type can be trivially mapped to integers as a separate step that is of no
interest to this discussion. The operations we use on that type are `zero`, a
value representing the "minimal" name (in our case, `0`), `succ`, an operation
that takes a name and returns the "next" one (`inc`), and "⊔", an operation
that takes two names are returns the bigger one (`max`).

If one were to sketch an implementation for `abs`, one would likely start with
something like:

```clojure
(defn abs
  [f]
  (let [n (generate-name f)]
    [:abs n (f [:var n])]))
```

Maybe it's not obvious that we need to pass the function `f` to
`generate-name`, and maybe we actually don't. At this point, I'm just
sketching, and `f` is the only bit of information I do have access to, so I
might as well assume I'll need to use it somehow.

What are the constraints on choosing a value for `n`? From the semantics of
lexical binding, we can deduce that it needs to:

1. Not capture free variables in the body of the function, `(f [:var n])`.
2. Should not be captured by other bindings in the body.

What does that mean? First, let's start with an example of 1. Say the body of
the function is:

```clojure
[:app [:var ???] [:var 2]]
```

where `???` is where our generated `n` should come into play. So we're going to
generate an abstraction of the form:

```clojure
[:abs n [:app [:var n] [:var 2]]]
```

Hopefully, it is pretty obvious that the meaning of this abstraction will be
very different if we set `n` to `2` than if we set `n` to any other number: if
we set it to `2`, we _capture_ the free variable, and end up with a function
that applies its argument to itself, rather than a function that applies its
argument to an externally-scoped value.

Now, where could that `[:var 2]` come from? If we construct expressions only
using `app` and `abs`, at first glance it looks like we cannot generate a _free
variable_. And that is true, at the level of a complete expression. But when
constructing an expression, subexpressions can (and will) have free variables.

For example, on our `+` operator above, when considering the abstraction that
introduces the binding `3`, all three other bindings look like free variables
to the body of that abstraction.

The second error case we want to avoid is to generate a binding that then gets
captured by a function "above" in the call stack.

This poses an obvious problem: because what we give `abs` is a _function_, the
only way it has of looking at the _expression_ of its body is to actually apply
the function and look at the result. Continuing with our example, we're
basically in a situation where we're trying to solve for:

```clojure
(abs (fn [n] [:app [:var n] [:var 2]]))
```

and it seems like the only way for `abs` to even see the expression is to pick
a number at random and execute the function. If we picked something other than
`2`, that's great, but if we picked `2`, we're screwed. Maybe we can run it a
bunch of times and see what values are stable?

### Threading state

The paper briefly considers the option of threading an infinite list of names
through (for which a [monadic implementation][monad-clj] may be useful), but
discard it as a non-starter, because that would require changing the base type
of expressions, i.e. the arguments and return types of the `app` and `abs`
functions now need to be wrapped in some way.

### Speculative naming

The first real solution to the problem that the paper presents is a more
disciplined approach to our "let's try random values and see what happens"
approach. The core idea is to decide that, in the final expression we produce,
the name `0` will never be used.

Under that assumption, the implementation is fairly straightforward:

```clojure
(defn app
  [e1 e2]
  [:app e1 e2])

(defn exp-max
  [exp]
  (match exp
    [:var n] n
    [:app e1 e2] (max (exp-max e1)
                      (exp-max e2))
    [:abs _ e] (exp-max e)))

(defn abs
  [f]
  (let [n (inc (exp-max (f [:var 0])))]
    [:abs n (f [:var n])]))
```

In other words, we first generate the expression while pretending we're going
to name the current variable `0`, then look at the expression, walking through
the entire thing in order to find out what the highest variable is, and then
generate our expression _again_ using a higher variable than that.

This works, but we call `f` twice at each level, meaning it's exponential in
the nesting level of our function definitions. That's obviously not ideal.




[lambda calculus]: /tags/paradigms
[interpreters]: /tags/cheap%20interpreter
[laziness]: /posts/2021-03-07-review-whyfp
[monads]: /tags/monad-tutorial
[monad-clj]: /posts/2021-10-03-monads-clojure
[epiphany]: /posts/2021-11-14-clojure-promise
[prime numbers]: /posts/2021-11-07-clj-primes
[paper]: https://emilaxelsson.github.io/documents/axelsson2013using.pdf
[abstract syntax]: /posts/2021-06-27-cwafi-2
[lambda]: /posts/2021-06-06-functional-church
