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
