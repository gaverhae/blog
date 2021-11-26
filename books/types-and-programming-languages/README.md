# Types and Programming Languages

[official page](https://www.cis.upenn.edu/~bcpierce/tapl/)

## Preface

The book focuses on practicality at the cost of completeness. It is designed in
a modular fashion, offering so many possible paths through it that the auther
provides a dependency graph between chapters.

## 1 - Introduction

### 1.1 - Types in Computer Science

For the purposes of this book,

> A type system is a tractable syntactic method for proving the absence of
> certain program behaviors by classifying phrases according to the kinds of
> values they compute.

Because type systems cannot prove the presence of errors, they have to reject
_some_ programs that would actually behave correctly at runtime. This creates a
tension between conservativity and expressiveness, which is the main driving
force pushing the field forward. More precise types allow for rejecting fewer
programs.

### 1.2 - What Type Systems Are Good For

- Detecting errors: the sooner an error is detected, the easier it is to fix.
  Type errors also tend to be very local, compared to runtime errors.
- Abstraction: interface and module boundaries are defined and checked by type
  systems.
- Documentation: types available to the reader of a program can be useful.
- Language safety: this is a tricky one to define as it's a bit language
  dependent, but types help, somehow.
- Efficiency: modern compilers tend to make use of static type information to
  optimize generated code.

### 1.3 - Type Systems and Language Design

It's hard to retrofit a type system on a language, so if the goal is to end up
with a type system, the language and its type system should be developed
together.

### 1.4 - Capsule History

The earliest type systems were invented in the fifties, and people have been
making them more complex ever since.

### 1.5 - Related Reading

Lots of references.

## 2 - Mathematical Preliminaries

### 2.1 - Sets, Relations, and Functions

Natural numbers include 0. A relation is a subset of a cartesian product
between n sets.

Subset (one-place relations) can be used as predicates, and will be used with
function notation (i.e. \(P(s)\) instead of \(s \in P\)).

Two-place relations use operator notation: \(s R t\) instead of \((s, t) \in
R\).

For larger relations, the text will use "mixfix" (SmallTalk-like) syntax, where
the relation name is chopped up and interspersed between the elements.

We distinguish partial functions from functions that fail: partial functions
are _not defined_ for some inputs, while functions can be _defined to fail_ on
some inputs, where failure is an observable result (i.e. different from
divergence). A function with failures but nott divergence can still be called
total if its definition domain is the entire "input" set.

On a single set, a predicate \(P\) is _preserved_ by a binary relation \(R\)
iff \(s R s' \and P(s) \implies P(s')\).

### 2.2 - Ordered Sets

A binary relation R on S can be reflexive (maps s to s for all s), symmetric
((s, t) implies (t, s)), transitive ((s,t) and (t, u) imply (s, u)), and/or
antisymmetric ((s, t) and (t, s) imply s == t).

A relation that is reflexive and transitive is a preorder. If it is also
antisymmetric, it is a partial order. If it covers all possible pairs, it is a
total order.

When we have a partial order, we can define least upper bounds and greater
lower bounds, which we respectively call join and meet.

A reflexive, transitive and symmetric relation is called an equivalence.

The reflexive closure of a relation is the smallest reflexive relation that
contains it. The transitive closure is the smallest transitive relation that
contains it.

> **Exercise.** Suppose we are given a relation R on a set S. Define R' as
> follows:
>
> R' = R \union {(s, s) | s \in S }
>
> That is, R' contains all the pairs in R plus all pairs of the form (s, s).
> Show that R' is the reflexive closure of R.

> By definition of a reflexive closure, we have three things to show:
>
> 1. R' is reflexive.
> 2. R' contains R.
> 3. R' is the smallest reflexive relation that contains R.
>
> 1 and 2 are trivially true by construction: R' is defined as being the union
> of R and something, so it contains R, and the something is exactly the
> reflexivity condition, so it is reflexive. To prove the third point, we need
> to construct a smaller set that still respects the first two points.
>
> Let's try removing an element from R'. If we remove an element of the form
> (s, s), the resulting relation is not reflexive anymore, so we cannot remove
> any of those. If we remove any of the other elements, the relation does not
> contain R anymore. Therefore, R' is the smallest possible reflexive relation
> that contains R, which makes it its reflexive closure.

> **Exercise.** Here is a more constructive defintion of the transitive closure
> of a relation R. First, we define the following sequence of sets of pairs:
>
> R\_0 = R
> R\_{i+1} = R\_i \union {(s, u) | for some t, (s, t) \in R\_i and (t, u) \in R\_i}
>
> That is, we construct each R\_{i+1} by adding to R\_i all the pairs that can
> be obtained by "one step of transitivity" from pairs already in R\_i.
> Finally, define the relation R^+ as the union of all the R\_i.
>
> Show that R^+ is really the transitive closure of R.

> By defintion, this means proving that:
> 1. R^+ is transitive.
> 2. R^+ contains R.
> 3. R^+ is the smallest relation that satisfies 1 and 2.
>
> 2 is evident by construction, as R^+ is defined as the union of a number of
> sets, among which R. For 1, the definition of transitive is that if (s, t)
> and (t, u) are in the relation, then (s, u) must also be in it. By
> construction, we have added all such (s, u), so the result is transitive.
>
> Is it the smallest possible one? Following the same logic as for the previous
> exercise, if we pick any element and remove it, that element is either:
> - An original element of R. Removing it means the result does not contain R
>   anymore.
> - An element we added in one of the R\_i steps. That means that the resulting
>   relation has two elements (s, t) and (t, u) for which (s, u) is not part of
>   the relation (we just removed it), and therefore the relation is not
>   transitive anymore.
> If we cannot remove an element without breaking either 1 or 2, it must follow
> that we have indeed the smallest relation that satisfies both 1 and 2.

> **Exercise.** Suppose R is a binary relation on a set S and P is a predicate
> on S that is preserved by R. Show that P is also preserved by R^\*.

> P is preserved by R if, whenever we have (s, t) in R and P(s), then P(t) is
> also true. R^\* is constructed by adding two types of elements to R:
> 1. To make the relation reflexive, we add "diagonal" elements (s, s). For
>    those, the property automatically holds; if P(s) is true, then P(s) is
>    also true. (Likewise, if false, it's still false.)
> 2. To make the relation transitive, we need to add "composition" elements: if
>    we _had_ (s, t) and (t, u), we add (s, u). But if we had (s, t) and P(s),
>    then we also know that P(t), and therefore P(u), which means the new tuple
>    still satisfies. If we did not have P(s), then we can add all the (s, x)
>    we want with no issue.

A _decreasing chain_ on a preorder Po on a set S is a sequence s1, s2, ... of
elements of S such that each s\_i < s\_{i+1} forall i. We call a set with a
preorder _well-founded_ if it has no infinite decreasing chain (e.g. the
natural numbers, opp. the integers). As a shortcut, we can say the set itself
is well-founded (if the preorder is obvious enough to be implicit).

### 2.3 - Sequences

A sequence is written by listin its elements. A sequence is a permutation of
another one if they contain the same elements, possibly in different orders. As
we deal mostly with non-nested sequences, we overload `,` to sequence both
individual elements and sequences (in a flattening way).

### 2.4 - Induction

Type theory makes use of proofs by induction: if things are true for 0 and i+1,
they are true for all numbers, etc. We use lexicographic order on pairs.

# Part I - Untyped Systems

## 3 - Untyped Arithmetic Expressions

### 3.1 - Introduction

We start with a simple, untyped language, summarized as:

```plaintext
t := 'false'
   | 'true'
   | 'if' t 'then' t 'else' t
   | '0'
   | 'succ' t
   | 'pred' t
   | 'iszero' t
```

Examples:

```plaintext
if false then 0 else 1
```

which evaluates to _"What's a '1'?"_, and

```plaintext
iszero (pred (succ 0))
```

which evaluates to `true`.

For our purposes, the interesting thing about this program is that its syntax
allows for expressions like

```plaintext
if 0 then 0 else 0
```

and

```plaintext
succ true
```

### 3.2 - Syntax

The syntax can more formally be defined as a set T such that:

0. T is the smallest set that satisfies the following rules:
1. {true, false, 0} \subseteq T,
2. if t\_1 \in T, then {succ t\_1, pred t\_1, iszero t\_1} \subseteq T,
3. if t\_1 \in T, t\_2 \in T, and t\_3 \in T, then "if t\_1 then t\_2 else t\_3" \in T.

Alternatively, we can construct T by starting with S\_0 being the empty set,
and defining S\_{i+1} to be:

```
{true, false, 0}
U {succ t1, pred t1, iszero t1 | t1 \in Si}
U {if t1 then t2 else t3 | t1, t2, t3 \in Si}
```

and saying T is the union of all Sis.

> **Exercise.** How many elements does S3 have?

> - S0 has zero elements.
> - S1 has 3 elements: the "atoms". The constructions add nothing because S0 is
>   empty.
> - S2 has the 3 basic elements, plus 9 elements for all the possible "depth
>   \<2" unary terms, and 27 elements for all the possible if-then-else
>   combinations of atoms, so 39 elements in total.
> - S3 has:
>   - 3 atoms.
>   - 3 times the size of S2 for all the "depth \<3" trees where the root is a
>     unary operator, so 117.
>   - The size of S2 cubed for all the possible if-then-elses: 59319.
>  for a total of 59439.

> **Exercise.** Show that the sets Si are cumulative (Si \subseteq S\_{i+1}).

> This is trivially true for i = 0, because every set contains the empty set.
> So let's assume it's true for i-1 and look at i. If Si contains S\_{i-1},
> then the construction of S\_{i+1} becomes:
> ```
> {true, false, 0}
> U {succ t1, pred t1, iszero t1 | t1 \in S_{i-1}}
> U {succ t1, pred t1, iszero t1 | t1 \in (Si \setminus S_{i-1})}
> U {if t1 then t2 else t3 | t1, t2, t3 \in S_{i-1}}
> U {if t1 then t2 else t3 | t1, t2, t3 \in (Si \setminus S_{i-1})}
> ```
> which clearly contains Si as a subset.

### 3.3 - Induction on Terms

The constructive approach to defining T (= S) allows us to define functions and
prove properties by induction on terms (i.e. pattern matching), because every
term is either a terminal or a is defined in terms of _smaller terms_.

For example:

```plaintext
consts(true) = {true}
consts(false) = {false}
consts(0) = {0}
consts(succ t) = consts(t)
consts(pred t) = consts(t)
consts(iszero t) = consts(t)
consts(if t1 then t2 else t3) = consts(t1) U consts(t2) U consts(t3)
```

> **Exercise.** Prove the principles of induction on terms:
> 1. Induction on depth: if, for each term s, given P(r) for all r such that
>    depth(r) < depth(s), we can show P(s), then P(s) holds for all s.
> 2. Induction on size: if, for each term s, given P(r) for all r such that
>    size(r) < size(s), we can show P(s), then P(s) holds for all s.
> 3. Structural induction: if, for each term s, given P(r) for all immediate
>    subterms r of s, we can show P(s), then P(s) holds for all s.

> 1. Going back to our constructive definition of T, the set of all r is
>    S\_{i-1}, and the set of all s is Si. Therefore, this follows directly
>    from induction on integers.
> 2. This requires a different way to construct our sets, but it similarly
>    boils down to induction on integers.
> 3. This is really just another way to phrase 1.

### 3.4 - Semantic Styles

There are three approaches to formalizing a language semantics:

1. _Operational semantics_ specifies the semantics by giving an abstract
   machine for it, which associates a transition function to possible states.
   The meaning of a term is defined as the final state the machine reaches when
   using that term as its initial state.
2. _Denotational semantics_ defines the meaning of a term to be some
   mathematical object, like a number or a function.
3. _Axiomatic semantics_ defines the meaning of a term as the set of properties
   that can be proven about it.

This book uses operational semantics.

### 3.5 - Evaluation

We can define the boolean subset of our language with the following operational
semantics:

```plaintext
t := true
   | false
   | if t then t else t
v := true
   | false
if true then t2 else t3 --> t2
if false then t2 else t3 --> t3
t1 --> t1' ==> if t1 then t2 else t3 --> if t1' then t2 else t3
```

where `t` is the form of all possible terms, `v` is the form of "values", a
special subset of terms that we admit as possible final states, and the rest
define an _evaluation relation_ where the term on the left of the `-->` can be
_reduced to_ the term on the right, provided that the condition before the
`==>` holds.

In other words, if the tuple `(t1, t1')` is in the evaluation relation, then
the tuple `(if t1 then t2 else t3, if t1' then t2 else t3)` is also in the
relation. Note that these rules implicitly define an evaluation order
(condition before consequent or alternative, in this case).

A term `t` is said to be "in normal form" if there is no evaluation step that
applies to it. For this language, normal forms coincide with values, but, in
general, values are a subset of normal forms. Normal forms that are not values
are "stuck" and represent errors, the kind we want to statically eliminate with
our type system(s).

> **Exercise.** Suppose we add a new rule:
> ```plaintext
> if true then t2 else t3 --> t3
> ```
> Which of the discussed properties remain valid?

> - Determinacy of one-step evaluation, "if `t --> t'` and `t --> t''`, then
>   `t' == t''`", is no longer valid.
> - Every value is still in normal form.
> - Every normal form is still a value.
> - Uniqueness of normal form, "if `t -->* u` and `t -->* u'`, then `u == u'`",
>   is no longer valid, as the proof relies on determinacy of one-step
>   evaluation.
> - Termination of evaluation is still true, as whichever path we take, we
>   still reduce the size of the expression by one.

> **Exercise.** Same question for the additional rule:
> ```plaintext
> t2 --> t2' ==> if t1 then t2 rlse t3 --> if t1 then t2' else t3
> ```

> - Determinacy of one-step evaluation does not hold.
> - Every value is still a normal form.
> - Every normal form is still a value.
> - Uniqueness of normal forms _does_ hold, but the proof needs to change:
>   instead of proving there is only one evaluation path, based on determinacy
>   of one-step evaluation, we now need to prove that all paths lead to the
>   same answer.
> - Termination of evaluation is still true, as every step still reduces the
>   size.

We now expand our evaluation rules with arithmetic:

```plaintext
t := true
   | false
   | if t then t else t
   | 0
   | succ t
   | pred t
   | iszero t
v := true
   | false
   | nv
nv := 0
    | succ nv
if true then t2 else t3 --> t2
if false then t2 else t3 --> t3
t1 --> t1' ==> if t1 then t2 else t3 --> if t1' then t2 else t3
t1 --> t1' ==> succ t1 --> succ t1'
pred 0 --> 0
pred (succ nv1) --> nv1
t1 --> t1' ==> pred t1 --> pred t1'
iszero 0 --> true
iszero (succ nv1) --> false
t1 --> t1' ==> iszero t1 --> iszero t1'
```

Note that determinacy of evaluation is preserved, as well as uniqueness of
normal forms, termination of evaluation, and the property that every value is
in normal form. However, there are now normal forms that are _not_ values, and
those are said to be _stuck_.

Instead of leaving the terms stuck, we could be more explicit by adding the
following definitions:

```plaintext
badnat := wrong
        | true
        | false
badbool := wrong
         | nv
if badbool then t1 else t2 --> wrong
succ badnat --> wrong
pred badnat --> wrong
iszero badnat --> wrong
```

> **Exercise.** Show that the two treatments are equivalent.

> For the two statements to be equivalent, they need to agree, i.e. each stuck
> term in the first approach needs to evaluate to `wrong` with the additional
> rules, and we must not have any non-stuck term that evaluats to `wrong`.
>
> The proof can be done by induction on the new rules: they only apply to stuck
> terms (i.e. no non-stuck term can evaluate to `wrong`), and they all evaluate
> to `wrong`. There is no stuck term that is not covered by the new rules.

The approach we've taken so far is known as "small step", where we define how
to perform one step of evaluation and then derive from that a notion of
repeated evaluation until we get a value (or we get stuck). A "big step"
appraoch would look like:

```plaintext
v --> v
t1 --> true AND t2 --> v2 ==> if t1 then t2 else t3 --> v2
t1 --> false AND t3 --> v3 ==> if t1 then t2 else t3 --> v3
t1 --> nv1 ==> succ t1 --> succ nv1
t1 --> 0 ==> pred t1 --> 0
t1 --> succ nv1 ==> pred t1 --> nv1
t1 --> 0 ==> iszero t1 --> true
t1 --> succ nv1 ==> iszero t1 --> false
```

The semantics coincide, in that if `t` derives to `v` in one approach, it will
derive to the same value in the other approach.

## 4 - An ML Implementation of Arithmetic Expressions

:shrug:

## 5 - The Untyped Lambda-Calculus

The lambda-calulus, invented by Alonzo Church in the 1920s and introduced to
computer science by Peter Landin in the 1960s, is a popular model for
computation because it can be viewed simultaneously as a programming language
in which computations happen and as a mathematical object about which rigorous
statements can be proven.

In general, a language can be understood as a smaller _core calculus_ and a
collection of _derived forms_ (also known as syntactic sugar_). The
lambda-calculus is a popular choice for the core calculus of a language, though
it is usually enriched in a variety of ways to make it more convenient, as
we'll see in later chapters. In this one, we focus on "pure" lambda calculus.

### 5.1 - Basics

Everything is a function. The syntax of the lambda-calculus comprises just
three sorts of terms:

- Variables: `[:var {x}]`, where `{x}` is a string, usually a single letter.
- Abstractions, `[:fn {x} {t}]`, where `{x}` is a string and `{t}` is a term.
- Applications: `[:app {t1} {t2}]`, where both `{t1}` and `{t2}` are arbitrary
  terms.

#### Abstract and Concrete Syntax

People hate parentheses and complicate their lives to get rid of them. Let's
not do that here.

Rather than memorizing the rules for parsing `λx.λy.xyx`, we'll just write
explicitly:

```clojure
[:fn "x" [:fn "y" [:app [:app [:var "x"] [:var "y"]] [:var "x"]]]]
```

#### Variables and Metavariables

Trying to get everything to be a single letter is confusing, so let's not do
that. When denoting metavariables, we'll just enclose them in braces. For
example, the term `[:fn "x" [:fn "y" [:app [:var "x"] [:var "y"]]]]` has
the form `[:fn {z} {s}]`, where `{z}` is `"x"` and `{s}` is `[:fn "y" [:app
[:var "x"] [:var "y"]]]`.

#### Scope

An occurrence of `[:var "x"]` is said to be _bound_ when it appears in the body
`{t}` of an abstraction `[:fn "x" {t}]`. It is _free_ if it is not bound. For
example, in `[:app [:fn "x" [:var "x"]] [:var "x"]]`, the first `x` is bound
and the second one is free.

A term with no free variables is _closed_ and is sometimes called a
_combinator_.

#### Operational Semantics

Each step consists of rewriting an application by replacing the appearances of
the variable in the body with the given argument. We write that process:

```clojure
(== (step [:app [:fn "x" {t1}] {t2}])
    (substitute "x" {t2} {t1}))
```

where `(substitute "x" {t1} {t2})` is the term obtained by replacing all _free_
occurrences of `x` with `{t2}` in `{t1}`. This is called beta-reduction.

In general, a single lambda term can contain many such reducible expressions
("redex"); how we choose which redex to reduce next is called an _evaluation
strategy_. _Full beta-reduction_ does all the possible reductions in any order
it pleases, which means single-step evaluation is not deterministic.

In the other strategies we'll mention, single-step evaluation is
deterministic._Normal order_ always starts with the leftmost, outermost redex.
_Call by name_ is similar but never descends into an abstraction; in effect it
can be seen as a variant of normal order that "stops earlier". In _call by
value_, used by most practical languages, we only evaluate the outermost
redexes (i.e. no descending into abstractions) and only after their argument
(`{t2}` in `[:app [:fn {x} {t1}] {t2}]`) has itself been fully evaluated.

Call by value is said to be _strict_ in the sense that the argument to a
function is always evaluated, regardless of whether the function ends up
actually using it.

This book uses call by value.

### 5.2 - Programming in the Lambda-Calculus

Lambda-calculus can be extended in various ways, but even in its simplest form,
it is very powerful. While staying within the bounds of the language, one can
fairly easily define conventions for representing multi-arg functions,
booleans, integers, and pairs.

It is, however, often useful to enrich the lambda calculus with more concrete
versions of those. In our case, we can do so by "merging" the syntax (and
evaluation rules) for lambda calculus with the boolean and arithmetic language
we have defined earlier.

This is our first language featuring divergence: some terms cannot be evaluated
to a normal form. For example:

```clojure
(def omega
  [:app [:fn "x" [:app [:var "x"] [:var "x"]]]
        [:fn "x" [:app [:var "x"] [:var "x"]]]])
```

Usually called `omega`, this lambda term one-step-evaluates to itself.

The operator `fix` lets us define recursive functions in a call-by-value
setting:

```clojure
(def fix
  [:fn "f"
    [:app [:fn "x" [:app [:var "f"]
                         [:fn "y" [:app [:app [:var "x"] [:var "x"]]
                                        [:var "y"]]]]]
          [:fn "x" [:app [:var "f"]
                         [:fn "y" [:app [:app [:var "x"] [:var "x"]]
                                        [:var "y"]]]]]]])
```

For example:

```clojure
(def factorial
  [:app [:var "fix"]
        [:fn "fct"
         [:fn "n"
          [:if [:app [:app [:var "realeq"] [:var "n"]]
                                           [:var "c0"]]
               [:var "c1"]
               [:app [:app [:var "times"] [:var "n"]]
                                          [:app [:var "fct"]
                                                [:app [:var "prd"]
                                                      [:var "n"]]]]]]]])
```

where `c0` and `c1` are, respectively, the Church representations of 0 and 1.

### 5.3 - Formalities

#### Syntax

Let V be a countable set of variable names. The set of terms is the smallest
set T such that:

1. Every element `"x"` of V is in T.
2. If `t1` is in T and `"x"` is in V, `[:fn "x" {t1}]` is in T.
3. If `t1` is in T and `t2` is in T, `[:app {t1} {t2}]` is in T.

The set of free variables of a term can be defined as:

```clojure
(defn free-variables
  [expr]
  (match expr
    [:var v] #{v}
    [:fn v t] (set/difference (free-variables t) #{v})
    [:app t1 t2] (set/union (free-variables t1)
                            (free-variables t2))))
```

> **Exercise.** Prove that \\(|FV(t)| \leq size(t)\\) for every `t`.

> Induction on the structure of `t`. If `t` is a variable, then |FV(`t`)| =
> size(`t`) = 1. If `t` is an abstraction, its size is size(`t1`) + 1, and
> |FV(`t`)| is either |FV(`t1`)| or |FV(`t1)| - 1, depending on whether `x`
> appears in `t1` or not. Finally, if `t` is an application, size is the sum
> plus one and |FV| is the sum.

#### Substitution

Substitution is a bit tricky to define formally, because of possible variable
name collisions. We need to distinguish between free and bound variables, be
careful about crossing abstractions, and be careful about capture.

Here is a working definition:

```clojure
(defn substitute
  [to-replace replacement expression]
  (match expression
    [:var to-replace] replacement
    (:or [:var other] [:fn to-replace t]) expression
    [:fn (other :guard (free-variable? replacement)) t]
      (substitute to-replace (alpha replacement other) expression)
    [:fn other t] [:fn other (substitute to-replace replacement t)]
    [:app t1 t2] [:app (substitute to-replace replacement t1)
                       (substitute to-replace replacement t2)]))
```

#### Operational Semantics

```plaintext
t := x
   | λx.t
   | t t
v := λx.t
t1 --> t1' ==> t1 t2 --> t1' t2
t2 --> t2' ==> v1 t2 --> t2'
(λx.t1) v2 --> [x -> v2] t1
```

This formalizes the evaluation order we have chosen.

## 6 -
