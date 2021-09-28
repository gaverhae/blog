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
S := t
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
relation.
