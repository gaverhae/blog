{:title "What is imperative programming?"
 :layout :post
 :tags ["paradigms"]}

In this post, I explain why you should care about Turing machines and what
they are. In the next parts of this series, I will contrast Turing machines
with lambda calculus and expand on why understanding both models is useful for
everyday programming.

## Functional v. object-oriented

Over the past fifteen years or so, fear of the flattening of Moore's law has
driven renewed interest in functional programming. Many programmers are still
unclear on what functional programming really is, and how it relates to their
current approaches, commonly designated as "object-oriented" programming.

This is, however, not a useful question. Functional code does not contrast with
object-oriented code, because they sit alongside different axes. They are
orthogonal to each other and, to a large extent, do not apply at the same
level.

In the next few posts, I'll try to explain what functional programming is, and
what it should be contrasted to. One of the biggest hurdles for such a
discussion is that programming, as a nascent discipline, does not have a
widely-agreed-upon jargon[^jargon] yet. For the purposes of this blog post, I
assume "object-oriented" is one of two things, which in my experience are at
the heart of the most common meanings for the term:

[^jargon]: Jargon is usually seen as a bad thing to the uninitiated, but the
development of jargon is actually a very important sign that a discipline is
maturing. Jargon is what lets experts in a field have fruitful discussions with
each other, where they can precisely refer to discipline-specific concepts and
ideas and reduce ambiguity and misunderstandings. Computer programming is still
a very young field of study; it has been pretty prolific in creating new words,
or appropriating existing words, but it has not yet reached the point where
those words truly have agreed-upon, precise definitions across the discipline.
Even basic terms like "variable" or "type" can mean widely different things to
different people. Object-oriented is definitely such a term, with at least one
definition per programming language that claims to be it.

- An organization of code whereby data and the functions to manipulate that
  data are colocated under a common reference. This is the nuts-and-bolt
  "objects and classes" language-level set of features.
- An architectural technique whereby systems are divided into separate parts
  that communicate through message-passing. This can be done in almost any
  language, or across languages if the messages are passed through some sort of
  network or queue.

## Models of computation

Functional programming, however, is a _model of computation_ based on Church's
"lambda calculus". It has pretty much nothing to do with either of the above
concepts. If a comparison needs to be made, it should be to _imperative
programming_, which is a model of computation based on Turing's "Turing
machine", and is inarguably the dominant programming paradigm of the last 200
hundred years.[^babbage]

[^babbage]: The field of computer programming is generally regarded as starting
in the 1950s, while the mathematical concept of a Turing machine was first
described by Turing in 1936. Still, Babbage's analytical engine was, at its
heart, a Turing machine, and thus imperative programming has been the dominant
paradigm since 1837.

So what's a model of computation? At the end of the 19th century, Cantor and
Russel broke mathematics.[^history] That lead mathematicians to question the
validity of their entire field, and thus to try and rigorously redefine
mathematics from the ground up in order to prove Cantor and Russel wrong. The
most important mathematical consequence from that effort is Gödel's
incompleteness theorems, which proved that mathematics was, and would remain,
broken. Mathematicians have since then made their peace with that state of
affairs. That thread of enquiry also led a few mathematicians to ponder the
notion of _computation_: Cantor's argument made a pretty good case for the
existence of real numbers that exist and can be precisely defined, yet cannot
be computed.

[^history]: This is obviously a very simplified version of history, with some
  inaccuracies thrown in for dramatic effect.

This was a bit unsettling, and thus prompted a handful of mathematicians to
ponder the deeper meaning of _computation_: what does it mean, in a precise,
rigorous mathematical sense, to compute a number? How can we tell the
difference between "this number cannot be computed" and "we don't know how to
compute that number yet"? One of the first proposition for such a formal
definition of a _model for computation_ was Church's lambda calculus. Having a
model does not really answer the question, though. All you can say with a model
is "this number can be computed _within this model_", but what if the model
itself is incomplete?

A few years later, Turing proposed his idea of a Turing machine, but more
importantly he offered a proof that the set of functions that can be described
with his Turing machine is exactly the same as the set of functions that can be
described with Church's lambda calculus. Since then, a fairly large number of
alternative models of computation have been proposed, and so far none of them
has been proven to be able to compute anything the Turing machine cannot
compute. In other words, the Turing machine is exactly as powerful as the
lambda calculus, and no other model we know of is more powerful (i.e. "can
compute more things") than either.

Note that we do _not_ have a proof that there is not a more powerful model of
computation available. All we know is that so far we have not found one. The
idea that the Turing machine _is_ the most powerful model of computation (as
opposed to "the most powerful one _we have found so far_) is known as "the
Church-Turing hypothesis".

Why am I telling you this? Mostly because I like to ramble, but also because
lambda calculus and Turing machines turn out to be really important for modern
computing, and because understanding them, even at a superficial level, can
give you deep insights into computer programming.

## Turing machines

Formally, a Turing machine is a 7-tuple \\(M=\\langle Q, \\Gamma, b, \\Sigma,
\\delta, q\_0, F\\rangle\\) where:

- \\(Q\\) is a finite set of _states_ (it has to be non-empty as it needs to
  contain at least \\(q\_0\\)).
- \\(\\Gamma\\) is an _alphabet_, i.e. a set of symbols (it has to be non-empty
  as it needs to contain at least \\(b\\)).
- \\(b\\in\\Gamma\\) is a special symbol of the alphabet, which can appear an
  infinite number of times on the _tape_ (see below for what the tape is); all
  other symbols in \\(\\Gamma\\) can only appear on the tape a finite number of
  times.
- \\(\\Sigma \\subseteq (\\Gamma \\setminus \\{b\\})\\) is the set of symbols
  that are allowed to appear on the _initial tape contents_.
- \\(q\_0\\in Q\\) is the initial state of the machine.
- \\(F\\subseteq Q\\) is the set of final states.
- \\(\\delta: (Q\\setminus F)\\times\\Gamma\\to
  Q\\times\\Gamma\\times\\{L,R\\}\\) is a (partial) function[^function] called
  the _transition function_.

[^function]: It may seem recursive that we invented Turing machines as a way to
  express functions, yet we need a function in the description of a Turing
machine. This is not actually a problem, because the purpose of a Turing
machine is _not_ to offer a way to define functions, but to offer a way to
describe how to compute functions. "Simple" functions can be described as a set
of pairs such that the first element is the input and the second element is the
output (and there are no two pairs with the same first element). The functions
that can be described in this way using a finite set of pairs do not need to be
computed, as they can just be described with their values directly available.
\\(\\delta\\) is such a function. Some functions cannot be described using a
finite set of pairs, but can still be described succinctly by a simple formula,
such as the function "x squared". But some functions have an infinite set of
inputs and no simple formula, such as square root.

This may or may not look like a jumble of meaningless symbols, and quite
frankly understanding the precise meaning of each of those is not very
important to my point here. (I do enjoy looking at typeset mathematics,
though.) These are just the components; a formal definition would also need to
proceed with exactly how they are used (i.e. what their semantics are), but I
prefer to drop the formality level a few notches at this point. For our
purposes, it is enough to know that a formal definition exists.

Informally, one can visualize a Turing machine as having three components:

- A _ribbon_, or _tape_, with things written on it. The ribbon has a starting
  point (generally assumed to be on the left), but it is infinite on the other
  side (generally the right side). It is one-dimensional, and is composed of
  cells, with exactly one symbol per cell. A given Turing machine defines the
  alphabet that can be used on the ribbon, \\(\\Gamma\\), but not the contents
  of the ribbon. This make sense if you think of the definition of the Turing
  machine as the formula for a function, and the (initial) content of the
  ribbon as the input to that function. When the machine "starts", the ribbon
  will contain a description of the input using symbols of \\(\\Sigma\\),
  followed by an infinite number of cells containing \\(b\\).
- A _head_, which is pointing to a given cell on the ribbon. It starts at 0
  (the "first" cell on the finite side of the ribbon).
- A "processor", which can direct the head to move either one cell to the right
  or one cell to the left, after having written a new symbol on the ribbon at
  its current position.

The operation of the processor is the core of the Turing machine, and is mostly
described by the \\(\\delta\\) function. The Turing machine can be thought of
as performing a number of _steps_, where each step consists of the following
operations:

1. Check the current state. If the current state is in \\(F\\), the machine has
   finished running. Stop here. Exactly what it means for the machine to have
   finished depends on how the machine was defined, but in most cases it means
   the output of the function is a combination of the current state of the
   machine and what is currently written on the ribbon.
2. Read the symbol on the ribbon, at the current position of the head. Apply
   the \\(\\delta\\) function, using as arguments the symbol that was just read
   and the current state of the machine. The result gives us a new state for
   the processor, a new symbol that we write on the ribbon (possibly the same
   one we just read), and a direction to move the head into.
3. GOTO 1.

Note that, while there is no "internal structure" to the states of the
processor in a Turing machine, this is not a limitation as far as
mathematicians care: any internal structure can just be expanded to a set of
individual states by enumerating all of the possible values of all internal
variables. For example, if you wanted your Turing machine state to be "any JSON
value representable using 30kb", that would be OK: this is a well-defined,
finite set of numbers[^json]. You could even decide to set the name of each
state to "the string representing that JSON value". This is not cheating. I
swear.

[^json]: Because JSON is defined as using UTF-8 encoding, any JSON
  representation maps directly to a well-defined, unique sequence of bytes,
  which can be seen as just one big number. I'm using "representation" here
  instead of "value" because there can be multiple representations for the same
  value, as JSON authorizes the use of non-significant whitespace.

Additionally, while the formal definition only allows the head to move one cell
at a time, it is trivial to implement larger moves by simply adding
intermediate states that do nothing but rewrite what they just read from it on
the ribbon and keep moving, perhaps decrementing a counter in their
non-existent-but-still-useable internal state representation.

As a concrete example, a Turing machine that does 2-bit addition could be
defined by:

- \\(Q := \\{B, B\_0, B\_1, B\_2, B\_3, H\_0, H\_1, H\_2, H\_3, H\\}\\)
- \\(\\Gamma := \\{0, 1, 2, 3, x\\}\\)
- \\(b := x\\)
- \\(\\Sigma := \\{0, 1, 2, 3\\}\\)
- \\(q\_0 := BB\\)
- \\(F := \\{H\\}\\)
- \\(\\delta\\) is defined by the following table:

<br />

| State | Symbol | to  | New state | Written symbol | Move |
|:-----:|:------:|:---:|:---------:|:--------------:|:----:|
| \\(B\\) | \\(0\\)| \\(\\to\\) | \\(B\_0\\) | \\(x\\)| \\(R\\)|
| \\(B\\) | \\(1\\)| \\(\\to\\) | \\(B\_1\\) | \\(x\\)| \\(R\\)|
| \\(B\\) | \\(2\\)| \\(\\to\\) | \\(B\_2\\) | \\(x\\)| \\(R\\)|
| \\(B\\) | \\(3\\)| \\(\\to\\) | \\(B\_3\\) | \\(x\\)| \\(R\\)|
| \\(B\_0\\) | \\(0\\)| \\(\\to\\) | \\(H\_0\\) | \\(x\\)| \\(L\\)|
| \\(B\_0\\) | \\(1\\)| \\(\\to\\) | \\(H\_1\\) | \\(x\\)| \\(L\\)|
| \\(B\_0\\) | \\(2\\)| \\(\\to\\) | \\(H\_2\\) | \\(x\\)| \\(L\\)|
| \\(B\_0\\) | \\(3\\)| \\(\\to\\) | \\(H\_3\\) | \\(x\\)| \\(L\\)|
| \\(B\_1\\) | \\(0\\)| \\(\\to\\) | \\(H\_1\\) | \\(x\\)| \\(L\\)|
| \\(B\_1\\) | \\(1\\)| \\(\\to\\) | \\(H\_2\\) | \\(x\\)| \\(L\\)|
| \\(B\_1\\) | \\(2\\)| \\(\\to\\) | \\(H\_3\\) | \\(x\\)| \\(L\\)|
| \\(B\_1\\) | \\(3\\)| \\(\\to\\) | \\(H\_0\\) | \\(x\\)| \\(L\\)|
| \\(B\_2\\) | \\(0\\)| \\(\\to\\) | \\(H\_2\\) | \\(x\\)| \\(L\\)|
| \\(B\_2\\) | \\(1\\)| \\(\\to\\) | \\(H\_3\\) | \\(x\\)| \\(L\\)|
| \\(B\_2\\) | \\(2\\)| \\(\\to\\) | \\(H\_0\\) | \\(x\\)| \\(L\\)|
| \\(B\_2\\) | \\(3\\)| \\(\\to\\) | \\(H\_1\\) | \\(x\\)| \\(L\\)|
| \\(B\_3\\) | \\(0\\)| \\(\\to\\) | \\(H\_3\\) | \\(x\\)| \\(L\\)|
| \\(B\_3\\) | \\(1\\)| \\(\\to\\) | \\(H\_0\\) | \\(x\\)| \\(L\\)|
| \\(B\_3\\) | \\(2\\)| \\(\\to\\) | \\(H\_1\\) | \\(x\\)| \\(L\\)|
| \\(B\_3\\) | \\(3\\)| \\(\\to\\) | \\(H\_2\\) | \\(x\\)| \\(L\\)|
| \\(H\_0\\) | \\(x\\)| \\(\\to\\) | \\(H\\) | \\(0\\)| \\(R\\)|
| \\(H\_1\\) | \\(x\\)| \\(\\to\\) | \\(H\\) | \\(1\\)| \\(R\\)|
| \\(H\_2\\) | \\(x\\)| \\(\\to\\) | \\(H\\) | \\(2\\)| \\(R\\)|
| \\(H\_3\\) | \\(x\\)| \\(\\to\\) | \\(H\\) | \\(3\\)| \\(R\\)|

<br />

There are missing entries in that table. For example, if the machine ever reads
a \\(2\\) symbol on the ribbon while in the \\(H\_0\\) state, the \\(\\delta\\)
function is undefined. In such cases, we say that the computation failed. This
is ok: \\(\\delta\\) is defined as a partial function to start with. If this
bothers you, though, you can simply add another state to \\(F\\) (and thus to
\\(Q\\)) representing failure, and add all the missing combinations of
\\(\Gamma\\) and \\(Q\\) to the table, mapping all of them to that new state
along with a movement to the right (which is always safe).

Also note how the set of states really represents two internal variables: where
we are in the computation (not read anything yet, need to read one more
variable, need to write result, done) and an integer accumulator to carry out
the actual addition.

In order to compute 2 + 1 using this Turing machine, we would set up the ribbon
with the following symbols:

\\[2, 1, x, x, \\dots\\]

The entire state of the computation could be written:

\\[B:\\{[2], 1, x, x, \\dots\\}\\]

where \\(B\\) represents the current state of the machine and \\([]\\)
represents the current position of the head. Using this notation, the
computation would proceed as follows:

1. \\(B:\\{[2], 1, x, x, \\dots\\}\\): \\(B\\) is not in \\(F\\), so we proceed.
   \\(\\delta(B, 2)\\to\\{B\_2, x, R\\}\\), so the next state is:
2. \\(B\_2:\\{x, [1], x, x, \\dots\\}\\): \\(B\_2\\) is not in \\(F\\), so we proceed.
   \\(\\delta(B\_2, 1)\\to\\{H\_3, x, L\\}\\), so the next state is:
3. \\(H\_3:\\{[x], x, x, x, \\dots\\}\\): \\(H\_3\\) is not in \\(F\\), so we proceed.
   \\(\\delta(H\_3, x)\\to\\{H, 3, R\\}\\), so the next state is:
4. \\(H:\\{3,[x],x,x,\\dots\\}\\): \\(H\\) _is_ in \\(F\\), so we stop.

We can now read the ribbon, and we have computed \\(2 + 1 = 3\\).

## Programmable machines

The example Turing machine above is running a specific program. This may seem a
bit limiting, but, as programmers, we know that if yoou can write a program in
a "Turing-complete" language, you can write a programmable program. This turns
out to be true of Turing machines. They are, after all, the very definition of
"Turing-complete".

That is, the model of a Turing machine is powerful enough to describe the model
of a Turing machine. In other words, it is possible to define a specific Turing
machine, generally called "the unversal Turing machine" (UTM), which can read a
description of a Turing machine and an input for it, and simulate the execution
of the described Turing machine on the given input. More precisely:

- The alphabet of the UTM is rich enough that one can describe any other Turing
  machine using it. (For example, that alphabet could be \\(\\{0, 1,
  \\epsilon\\}\\).)
- That definition would include the specification of the input alphabet of the
  "simulated" Turing machine, or an encoding thereof.
- A given run of the UTM would correspond to a given run of the simulated
  machine _on a specific input for that machine_; therefore the initial ribbon
  of the UTM can be described as containing two parts: first, a description of
  the simulated Turing machine, and second, the input for the simulated Turing
  machine.

The UTM can simulate itself simulating another machine, etc., though it
obviously gets really tedious really fast.

Why is any of this relevant? If you squint a little bit, a Turing machine is a
really good description of a programmer trying to understand imperative code.
As an example, let's take a look at the following C code:

```c
/* 1 */  int x = 1;
/* 2 */  int i = 1;
         do {
/* 3 */    x = x * i;
/* 4 */    i = i + 1;
/* 5 */  } while (i <= 4);
/* 6 */  printf("%d\n", x);
```

In order to predict what will be printed, a programmer would read the code one
line at a time, updating their internal state of belief for what the value of
each variable is at each step. Calling \\(H\\) the current line (for _head_),
here is how such an evaluation would go:

- \\(\\{x = ?, i = ?, H = 1\\}\\): `x = 1` we need to set \\(x\\) to \\(1\\).
- \\(\\{x = 1, i = ?, H = 2\\}\\): `i = 1` we need to set \\(i\\) to \\(1\\).
- \\(\\{x = 1, i = 1, H = 3\\}\\): `x = x * i` we need to set \\(x\\) to \\(1\\).
- \\(\\{x = 1, i = 1, H = 4\\}\\): `i = i + 1` we need to set \\(i\\) to \\(2\\).
- \\(\\{x = 1, i = 2, H = 5\\}\\): `i <= 4` is true so we set \\(H\\) to \\(3\\).
- \\(\\{x = 1, i = 2, H = 3\\}\\): `x = x * i` we need to set \\(x\\) to \\(2\\).
- \\(\\{x = 2, i = 2, H = 4\\}\\): `i = i + 1` we need to set \\(i\\) to \\(3\\).
- \\(\\{x = 2, i = 3, H = 5\\}\\): `i <= 4` is true so we set \\(H\\) to \\(3\\).
- \\(\\{x = 2, i = 3, H = 3\\}\\): `x = x * i` we need to set \\(x\\) to \\(6\\).
- \\(\\{x = 6, i = 3, H = 4\\}\\): `i = i + 1` we need to set \\(i\\) to \\(4\\).
- \\(\\{x = 6, i = 4, H = 5\\}\\): `i <= 4` is true so we set \\(H\\) to \\(3\\).
- \\(\\{x = 6, i = 4, H = 3\\}\\): `x = x * i` we need to set \\(x\\) to \\(24\\).
- \\(\\{x = 24, i = 4, H = 4\\}\\): `i = i + 1` we need to set \\(i\\) to \\(5\\).
- \\(\\{x = 24, i = 5, H = 5\\}\\): `i <= 4` is **false** so we set \\(H\\) to \\(6\\).

And the program reaches its end. Note how closely this resembles the workings
of a Turing machine: each numbered line of the program could be a symbol from
the input alphabet, and the transition between each bullet point can easily be
encoded as a \\(\\delta\\) function. As described, the value of all variables
can also be easily encoded into a state representation.

## CPUs

This correspondence between Turing machines and C code is no accident. The
reason Turing machines are the most important model of computation we have is
because _they can be built_. CPUs really are just physical implementations of a
universal Turing machine (albeit with a finite ribbon of memory).

> Like a Turing machine, _imperative code_ is _executed_ by reading
> instructions that tell:
> 1. How to update the internal state of a processor, and
> 2. Where to look for the next instruction.

## Up next

This is why imperative code is the dominant programming paradigm: we started
with a handful of models of computation, were able to build one of them, and
then wrote programs for that one. Over time, we added layers of abstraction,
but ultimately we mostly remained stuck within the confines of the underlying
hardware. Importantly, note that this story does not say we're programming
Turing machines because it's easy, or good. It simply was _possible_.

In the next post, I will describe another model of computation, lambda
calculus, how it relates to functional programming, and why we can and should
finally break the shackles of the Turing machine domination.
