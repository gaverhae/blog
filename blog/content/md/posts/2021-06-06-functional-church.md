{:title "What is functional programming?"
 :layout :post
 :tags ["paradigms"]}

[Last week][turing] I wrote a concise, informal introduction to Turing machines
and the associated programming model, imperative programming. In this post, I
present a similarly informal introduction to lambda calculus and the associated
"functional programming" model.

## Lambda calculus

Lambda calculus is formally defined as performing _reduction operations_ on
_lambda terms_, where lambda terms are built using these:

- There is an alphabet of symbols representing _variables_ (usually single
  letters).
- An expression of the form \\((\\lambda x. M)\\), where \\(x\\) is a variable
  and \\(M\\) is a term, is a function definition, also called an
  _abstraction_.
- An expression of the form \\((M N)\\) where both \\(N\\) and \\(M\\) are
  terms is the _application_ of the function \\(M\\) evaluated to the
  argument \\(N\\).

Here are a few examples of lambda terms:

- \\(a\\)
- \\((\lambda x. x) a\\)
- \\((\lambda f. ((\lambda g. (g g))(\lambda g. (f (\lambda a. ((g g) a))))))\\)

Note that our definition of lambda terms implicitly mandates full
parenthesisation (that's probably not a word, but I'm sure you know what I
mean). This is, however, unnecessary, because under lambda calculus evaluation
rules, function application is associative. That is, \\(((\lambda x.
B[x])(\\lambda y.  C[y]))a\\) reduces to the same expression as \\((\lambda x.
B[x])((\lambda y.  C[y])a)\\) would. Here I am using \\(B[x]\\) as a shorthand
for "some lambda term in which \\(x\\) appears; this is not part of lambda
calculus syntax, just a shorthand for generalisation.

The above expressions would thus more commonly be written:

- \\(a\\)
- \\((\lambda x. x) a\\)
- \\(\lambda f. ((\lambda g. gg)(\lambda g. f (\lambda a. g g a)))\\)

I mentioned "evaluation rules"; you may be wondering what those are. I was
referring to the _reduction operations_ mentioned above, which are:

- \\(\\lambda x. M[x]\\) can be rewritten to \\(\\lambda y. M[y]\\). This is
  called \\(\alpha\\)-conversion. The notation \\(M[x]\\) here means the
  variable \\(x\\) appears as a _free variable_ in the term \\(M\\). This is
  mostly there to reduce confusion for humans who might be confused by name
  collisions; in an ideal world, every single variable is unique throughout the
  entire expression being evaluated.[^indices]
- \\(((\\lambda x. M[x]) E) \\to (M[x := E])\\), called \\(\\beta\\)-reduction,
  consists in replacing all of the _bound_ instances of \\(x\\) in \\(M\\) with
  the term \\(E\\).

[^indices]: There is actually an [alternative notation][de bruijn] for lambda
  calculus that uses unambiguous indices instead of variable names. It's great
  for machines, but it gets a bit tricky for humans.

The notions of _free_ and _bound_ variables stem from the ambiguity of using
letters for variable names. For example, in the lambda term \\(\lambda x.
(\lambda x. x)\\), using only the rules we have discussed so far, we cannot
resolve the innermost \\(x\\). This is a well-known issue in almost any
programming language, and it is resolved by the introduction of _scoping
rules_. Lambda calculus being arguably the first ever programming language, the
notion of scoping rules was not immediately obvious[^scoping], but the original
intent was that it should work as lexical scope is commonly understood these
days. That is, that term is equivalent to \\(\lambda x_1.(\lambda x_2.x_2)\\).

[^scoping]: In fact, the original Lisp implementation was an attempt at
  implementing lambda calculus, but used the wrong scoping rules, commonly
known as "dynamic" scope.

Informally, in \\(\lambda x. \lambda y . xyz\\), we say that \\(x\\) and
\\(y\\) are _bound_, because they correspond to the argument of an enclosing
abstraction, whereas \\(z\\) is _free_, because it is not bound by an enclosing
abstraction. Similarly, in \\(\lambda x. ((\lambda y. xyz)y)\\), the innermost
\\(y\\) is bound, but the last one _is not_, and it is important to realize
that they are not the same \\(y\\).

Let's walk through an example of applying those reductions. Consider the term:

\\[(\\lambda x. x)(\\lambda x. x)z\\]

It can be rewritten to just \\(z\\) through the following sequence of steps:

- We start with:<br/>
  \\((\\lambda x. x)(\\lambda x.x) z\\)
- \\(\\alpha\\)-reduction to rename \\(x\\) to \\(y\\) in the second term,
  mostly for clarity:<br/>
  \\((\\lambda x. x)(\\lambda y.y) z\\)
- \\(\\beta\\)-reduction on \\(y\\):<br/>
  \\((\\lambda x. x) z\\)
- \\(\\beta\\)-reduction on \\(x\\):<br/>
  \\(z\\)

Because we did not fully parenthesise the original term, it is ambiguous, and
could also be interpreted as associating to the left, yielding:

- We start with:<br/>
  \\((\\lambda x. x)(\\lambda x.x) z\\)
- \\(\\alpha\\)-reduction to rename \\(x\\) to \\(y\\) in the second term,
  mostly for clarity:<br/>
  \\((\\lambda x. x)(\\lambda y.y) z\\)
- \\(\\beta\\)-reduction on \\(x\\):<br/>
  \\((\\lambda y. y) z\\)
- \\(\\beta\\)-reduction on \\(y\\):<br/>
  \\(z\\)

Note that associativity is not the only reason why there may be multiple ways
to reduce a lambda term. In the general case of \\((M E)\\), there is nothing
in the definition of lambda calculus that prescribes whether one should first
completely reduce \\(E\\) (or \\(M\\)) before doing the top-level
\\(\\beta\\)-reduction.

## Extending lambda calculus

It should be pretty obvious that the lambda calculus, as a model, is a lot
simpler than the Turing machine. It is composed of fewer moving parts, and, more
importantly, while it follows a different set of rules from normal arithmetic,
it does look like "normal" computation. Compare the above evaluation with:

- We start with:<br/>
  \\(1 + 2 * 3\\)
- evaluating \\(\*\\):<br/>
  \\(1 + 6\\)
- evaluating \\(+\\):<br/>
  \\(7\\)

Simplicity is nice, but only if it comes without a loss of power. I've already
claimed that lambda calculus is exactly as powerful as Turing machines, but at
this point it may be hard to see how that can be the case.

We can extend lambda calculus, through mathematical trickery, without
compromising its intrinsic qualities and without making it more complex. That
is, just by defining additional, shorthand notations that always have a clear
mapping to the underlying "low-level" lambda calculus operations.

For example, one can define numbers by first mapping the numeric symbols to
lambda terms:

- \\(0\\to\\lambda f.\\lambda x.x\\)
- \\(1\\to\\lambda f.\\lambda x. f x\\)
- \\(2\\to\\lambda f.\\lambda x. f f x\\)
- \\(3\\to\\lambda f.\\lambda x. f f f x\\)
- \\(\\dots\\)

and then mapping common arithmetic operations in such a way that it works as
expected across those representations. For example addition is defined as:

- \\(+\\to\\lambda m.\\lambda n.\\lambda f.\\lambda x.mf(nfx)\\)

Coming up with that definition is definitely not trivial, and it took
mathematicians a lot of effort to figure this out. But now that it's there we
can just look it up and use it.

We can compute \\(2 + 1\\) with:

- First, write it with parentheses:<br/>
  \\((+ 2) 1\\)
- replacing \\(+\\):<br/>
  \\(((\\lambda m.\\lambda n.\\lambda f.\\lambda x.mf(nfx)) 2)1)\\)
- \\(\\beta\\)-reduction on \\(m\\):<br/>
  \\((\\lambda n. \\lambda f. \\lambda x. 2f(nfx))1\\)
- \\(\\beta\\)-reduction on \\(n\\):<br/>
  \\(\\lambda f. \\lambda x. 2f(1fx)\\)
- At this point, if we were to replace \\(2\\) or \\(1\\) with their definition, we may end up with confusing uses of \\(f\\) and \\(x\\). But under our definition of equality, we don't _have_ to name them that, we can pick any name (because of \\(\\alpha\\)-reductions). So, replacing \\(f\\) and \\(x\\) in the definition of \\(2\\) with \\(g\\) and \\(y\\), we get:<br/>
  \\(\\lambda f. \\lambda x. (\\lambda g.\\lambda y. ggy)f(1fx)\\)
- Similarly, we replace \\(f\\) and \\(x\\) in \\(1\\) with \\(h\\) and \\(z\\)<br/>
  \\(\\lambda f. \\lambda x. (\\lambda g. \\lambda y. ggy)f((\\lambda h. \\lambda z. hz)fx)\\)
- \\(\\beta\\)-reduction on \\(h\\):<br/>
  \\(\\lambda f. \\lambda x. (\\lambda g. \\lambda y. ggy)f(\\lambda z. fz)x\\)
- \\(\\beta\\)-reduction on \\(z\\):<br/>
  \\(\\lambda f. \\lambda x. (\\lambda g. \\lambda y. ggy)ffx\\)
- \\(\\beta\\)-reduction on \\(g\\):<br/>
  \\(\\lambda f. \\lambda x. (\\lambda y. ffy)fx\\)
- \\(\\beta\\)-reduction on \\(y\\):<br/>
  \\(\\lambda f. \\lambda x. fffx\\)
- and that is our representation for \\(3\\).

You may think that the "_replacing X_" steps are cheating, as lambda calculus
as defined does not have any mechanism for "global variables". We can work
around that. Observe that, in the original expression above, \\(+\\), \\(1\\),
and \\(2\\) are free variables. We can simply _bind_ them by enclosing the
original expression in an abstraction and applying it. Thus, the original
expression \\((+2)1\\) can be seen as a shorthand notation for what would
formally be:

\\[(\\lambda +. (\\lambda 2. (\\lambda 1. ((+ 2) 1))))(\\lambda m.\\lambda n.\\lambda f.\\lambda x.mf(nfx))(\\lambda f.\\lambda x. f f x)(\\lambda f.\\lambda x. f x)\\]

or, less formally:

\\[(\\lambda +. \\lambda 2.\\lambda 1. + 2 1)(\\textrm{def. of }+)(\\textrm{def. of } 2)(\\textrm{def. of }1)\\]

## Programming languages

Just like we can recover numbers, we can recover booleans and an if-then-else
construct. Additionally, it is possible to define anonymous recursion using the
Y combinator. This means that, through simple rewrite rules, we can extend the
notation for lambda calculus, _without adding anything to the underlying
model_, to become a programming language with:

- Global, potentially recursive function definitions.
- Integers and arithmetic.
- Conditionals.

It should be pretty clear that integers are enough to redefine any other data
type we want, which means that this yields a useable programming
language[^pure]. The Lisp family of languages is directly inspired by lambda
calculus, and Scheme provides a nice ASCII notation for our extended lambda
calculus.  For example:

[^pure]: Well, except for the part where it's _pure_; it's a programming
  language that can be "pleasantly" and "productively" used to compute any
  function, but which cannot be used for any _program_.

```scheme
(define factorial
  (lambda (x)
    (if (= 1 n)
      1
      (* n (factorial (- n 1))))))
```

and, because this can be seen as just a thin syntactic veneer on top of lambda
calculus, this evaluates in pretty much the same way:

- `(factorial 4)`: replacing `factorial` with its definition.
- `((lambda (n) (if (= 1 n) 1 (* n (factorial (- n 1))))) 4)`: \\(\\beta\\) reduction.
- `(if (= 1 4) 1 (* 4 (factorial (- 4 1))))`: evaluating `(= 1 4)`.
- `(if false 1 (* 4 (factorial (- 4 1))))`: applying `if`.
- `(* 4 (factorial (- 4 1)))`: evaluating `(- 4 1)`.
- `(* 4 (factorial 3))`: replacing `factorial` with its definition.
- `(* 4 ((lambda (n) (if (= 1 n) 1 (* n (factorial (- n 1))))) 3))`: \\(\\beta\\) reduction.
- `(* 4 (if (= 1 3) 1 (* 3 (factorial (- 3 1)))))`: evaluating `(= 1 3)`.
- `(* 4 (if false 1 (* 3 (factorial (- 3 1)))))`: applying `if`.
- `(* 4 (* 3 (factorial (- 3 1))))`: evaluating `(- 3 1)`.
- `(* 4 (* 3 (factorial 2)))`: replacing `factorial` with its definition.
- `(* 4 (* 3 ((lambda (n) (if (= 1 n) 1 (* n (factorial (- n 1))))) 2)))`: \\(\\beta\\) reduction.
- `(* 4 (* 3 (if (= 1 2) 1 (* 2 (factorial (- 2 1))))))`: evaluating `(= 1 2)`.
- `(* 4 (* 3 (if false 1 (* 2 (factorial (- 2 1))))))`: applying `if`.
- `(* 4 (* 3 (* 2 (factorial (- 2 1)))))`: evaluating `(- 2 1)`.
- `(* 4 (* 3 (* 2 (factorial 1))))`: replacing `factorial` with its definition.
- `(* 4 (* 3 (* 2 ((lambda (n) (if (= 1 n) 1 (* n (factorial (- n 1))))) 1))))`: \\(\\beta\\) reduction.
- `(* 4 (* 3 (* 2 (if (= 1 1) 1 (* 1 (factorial (- 1 1)))))))`: evaluating `(= 1 1)`.
- `(* 4 (* 3 (* 2 (if true 1 (* 1 (factorial (- 1 1)))))))`: applying `if`.
- `(* 4 (* 3 (* 2 1)))`: evaluating `(* 2 1)`.
- `(* 4 (* 3 2))`: evaluating `(* 3 2)`.
- `(* 4 6)`: evaluating `(* 4 6)`.
- `24`.

> Like lambda calculus, _functional code_ is _evaluated_ by applying these
> three reduction operations on an expression:
> - Replacing a name by its definition.
> - \\(\\beta\\)-reduction of a lambda term (function application).
> - Evaluation of higher-level abstractions (if-then-else, +, etc.).
>
> There is no state to update, no need to keep track of "variable values":
> everything is embedded in the expression itself, at every step of evaluation.

In a very real way, an expression in a (purely) functional language _is_ a
lambda term. Functional programming _is_ lambda calculus, just with a slightly
enriched notation.

## Why lambda calculus?

Last week we looked at Turing machines, which I argued were important because
_they can be built_. In contrast, lambda calculus is important because _it is
composable_, and that means it can be used to _manage complexity_, which is at
the heart of everything we want to do in software engineering.

In this context, _composable_ refers to the fact that a lambda term is built
out of other lambda terms, yielding a _tree_ where every subtree is itself a
valid lambda term. One can reason about individual subtrees without considering
the entirety of the program. One can also easily reuse subtrees if one is known
to do a specific task, in the same way we added numbers and \\(+\\) to our
language above. Once a given lambda term is known to be useful, it is trivial
to use it in constructing other lambda terms.

In other words, reasoning about lambda calculus is equivalent to reasoning
about a single lambda term at a time. If you think of a lambda term as a tree,
you can take any subtree and still be able to reason about it, and its current
state at this point in the computation, in isolation. This decomposability is
where the power comes from.

In contrast, in order to reason about the current
state of a Turing machine, you need to know the entire program (\\(\\delta\\)),
the entire current content of the ribbon, the current position of the head and
the current state of the machine. There is no obvious, general way to think
about a subset of a running Turing machine. If you find a useful Turing
machine, such as the 2-bit adder I showed in the [previous post][turing], it is
completely unclear how to reuse that within another Turing machine.

Lambda calculus may seem like it starts a lot smaller and, in a way, poorer
than a Turing machine (because it does), but it is fundamentally _extensible_
through composition and that makes it a lot more powerful[^power] in practice.

[^power]: I have mentioned multiple times that Turing machines and lambda
  calculus are, from a theoretical perspective, _exactly as powerful as each
  other_, by which I mean that they can compute the exact same set of numbers, or
  alternatively that they can represent the exact same set of functions. In this
  context, when I write that lambda calculus is _more powerful_, I really mean
  "more pleasant to use for a human", i.e. that the vast majority of these
  functions are easier to represent in lambda calculus than using Turing machines,
  even though they _can_ be represented with both.

## Up next

In the next post in this series, I will take a look at the relationship between
lambda calculus and Turing machines, and what that relationship means for
everyday programming (yes, there is a point to all this).

[turing]: /posts/2021-05-30-imperative-turing
[de bruijn]: https://en.wikipedia.org/wiki/De_Bruijn_index
