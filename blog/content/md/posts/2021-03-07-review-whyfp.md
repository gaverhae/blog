{:title "Review: Why FP Matters (Hughes)"
 :layout :post
 :tags ["papers" "fp"]}

### "Why Why FP Matters Matters"

A few years ago, I came across the blog post [Why Why Functional Programming
Matters Matters]. It was a major factor in my decision to invest heavily into
learning functional programming, so it's safe to say it's had a huge impact on
my development as a software engineer. In fact, I highly recommend reading
everything [Reginald Braithwaite] has ever written; I can't claim to have read
it all, but I can say that I've never regretted any minute spent doing it.

Today, however, I want to talk about the [paper] that blog post talks about. It
is titled "_Why Functional Programming Matters_", and if you have never read
it, you should go read it right now. Don't worry, I'm not going anywhere. As
a piece of static text, I have all the patience in the world.

As a brief summary, the paper argues that functional programming yields better
programs because it adds constraints on programs, which in turn lets the
programmer make assumptions about program segments (i.e. functions), which
increases reasoning ability, and thus composability, and therefore makes it
possible to use new types of "glue" between code segments, thereby improving
code reuse. I'm not going to repeat all of the arguments, you just read them.
(Seriously, go read the paper if you haven't.)

A couple weeks ago I decided to reread the paper, to check if everything I've
learned in the past dozen years since I first read it would allow me to gather
any new insight. I decided that a superficial reading would not be enough, and
that to force me to really carefully read everything I would reimplement the
code samples. I also thought this would be a nice test of how [Clojure] holds
up to Hughes' definition of functional programming.

This has led me to a few realizations that I had not made while simply reading
the paper, which is the subject of this blog post. If you're interested in more
details than are presented here, you can take a look at the entire Clojure code
[here].

### Types

In modern days, [Haskell] has taken the mantle of the Avatar of Functional
Programming, to the point that many programmers do not meaningfully distinguish
"functional programming" from "strong static typing". This is obviously a
problem for Clojure as it is, as the scholars say in a disdain-dripping tone,
_untyped_.

Interestingly, Hughes' paper does not discuss static typing at all, and does
not seem to include it in his definition of functional programming. Instead, he
defines functional programming as the absence of an assignment operator.[^1]

Furthermore, while it may look superifically like Haskell, the language used in
the paper (Miranda) does not seem to respect our modern notions of static
typing either. For example, we can extract the following code snippet from the
paper:

```
listof * ::= Nil | Cons * (listof *)
treeof * ::= Node * (listof (treeof *))
```

which would roughly corresponf to the Haskell type definitions:

```haskell
data List a = Nil | Cons a (List a)
data Tree a = Node a (List (Tree a))
```

So far, so good. But then Hughes writes this function to fold over a tree:

```
foldtree f g a (Node label subtrees) = f label (foldtree f g a subtrees)
foldtree f g a (Cons subtree rest) = g (foldtree f g a subtree)
                                       (foldtree f g a rest)
foldtree f g a Nil = a
```

This really threw me off and it took me a while to realize that the reason I
had trouble translating that function to a meaningful implementation in my
Clojure code was that is it not properly typed: the fourth argument is assumed
to be either a Tree or a Node. Knowing that, it becomes trivial to implement,
but I was certainly not expecting my source material to have type issues.

### Higher-order functions

According to Hughes, the first new type of glue that is enabled by the absence
of assignments — or, more generally, side-effects — is higher-order functions.
I do not fully agree with that, as it seems perfectly possible to use
higher-order functions in an imperative language, with some of the functions
performing side-effects.

Most of the functions defined in Section 3 of the [paper] translate fairly
directly to Clojure. For example:

```
(foldr f x) Nil = x
(foldr f x) (Cons a l) = f a ((foldr f x) l)
```

can be written in "pure" Clojure as:

```clojure
(defn foldr
  [f init]
  (fn [ls]
    (if (empty? ls)
      init
      (f (first ls) ((foldr f init) (rest ls))))))
```

or, if we want to get closer to the pattern matching syntax used in the paper,
using [core.match]:

```clojure
(defn foldr
  [f init]
  (fn [ls]
    (match ls
      ([] :seq) init
      ([hd & tl] :seq) (f hd ((foldr f init) tl)))))
```

The main sticking point throughout this section is currying: Clojure has
multi-argument functions, and thus no automatic currying. This is why we have
to explicitly return a function here.

The previously-mentioned `foldtree` function is interesting:

```
foldtree f g a (Node label subtrees) = f label (foldtree f g a subtrees)
foldtree f g a (Cons subtree rest) = g (foldtree f g a subtree)
                                       (foldtree f g a rest)
foldtree f g a Nil = a
```

A direct implementation of this in Clojure would read:

```
(defn foldtree
  [node-fn list-fn zero [node children]]
  (node-fn node
           (reduce list-fn
                   zero
                   (map #(foldtree node-fn list-fn zero %) children))))
```

assuming a representation of lists as Clojure vectors and nodes as two-element
vectors.

This lets us write the last function of Section 3, `maptree`:

```
maptree f = foldtree (Node . f) Cons Nil
```

as follows:

```clojure
(defn maptree
  [f]
  #(foldtree
     (fn [node children] [(f node) children])
     conj [] %))
```

Again, currying is the main difference here: `(Node . f)` is a succint way to
express a function that takes one argument, applies `f` to it, then waits for a
second argument to construct a Node. With our chosen representation of nodes as
tuples, the Clojure equivalent of the `Node` function would be `vector` called
with two arguments, and while Clojure does have a direct equivalent for `.`
(`comp`), there is no direct syntax for delaying evaluation until another
argument is provided. The most faithful transcription of `(Node . f)` would be
to explicitly curry `vector`:

```clojure
(comp (fn [x] (fn [y] (vector x y))) f)
```

Overall, if we're willing to wrap all of our functions in this way (including
redefining the standard library functions), all of the examples of Section 3
translate in a very direct way.

One interesting point that does not really apper until we try to reuse
`maptree` in Section 5 is that there is no good way to write a lazy `foldr` in
Clojure. Clojure does have some support for laziness in the form of lazy
lists[^2], but the language overall is strict.

To be fully lazy, `foldr` needs the function `f` itself to be lazy in its
second argument, which cannot be generally expected of Clojure functions. If we
were ready to rewrite all our functions, as we could do for currying, we could
have `foldr` looking something like:

```clojure
(defn foldr
  [f init]
  (fn [ls]
    (match ls
      ([] :seq) init
      ([hd & tl] :seq) (f hd (delay ((foldr f init) tl))))))
```

but `f` would need to know to `force` its second argument, and that gets messy
very quickly.

This limited support for laziness may not be as problematic as one might at
first think, though. Consider, for example, the function `sum` described in the
paper:

```
sum = foldr (+) 0
```

which readily translates to

```clojure
(def sum (foldr + 0))
```

Even in Miranda (or Haskell), this is not lazy in any meaningful sense: the `+`
function is not able to return a partial result and will always need to fully
evaluate both of its arguments. Most use-cases of `foldr` where we want
laziness are actually cases where the expected result is a list. In a
lazy language, such lazy list operations _can_ be built atop `foldr` using the
laziness of `cons`, but realistically they are provided by the standard library
(`map`, `filter`, etc.) and what they are built upon is only of limited
relevance. These operations can be built lazily even in the absence of a lazy
fold.

For example, the paper builds a function `maptree` defined by:

```
maptree f = foldtree (Node . f) Cons Nil
```

Just like `foldr`, it is pretty hard to build a lazy `foldtree` in Clojure.
However, the operations for which that matters are rare, and examples like
`maptree` can easily be built to be lazy by not building them on top of fold:

```clojure
(defn maptree
  [f [value children]]
  [(f value) (map #(maptree f %) children)])
```

This would translate in Miranda to:

```
maptree f (Node value children) = Node (f value) (map (maptree f) children)
```

which, even in Miranda, I like better than the version using `foldtree`.

### Lazy lists

Section 4 of the paper is ostensibly about numerical computation, but is really
about lazy lists. As I would expect, Clojure stands up very well here, as it
has good support for lazy lists to start with and the code in this section is
not too reliant on currying. For example, the square root computation is given
in the paper as:

```
next n x = (x + n/x) / 2
repeat f a = Cons a (repeat f (f a))
relative eps (Cons a (Cons b rest))
  = b, if abs (a/b - 1) <= eps
  = relative eps (Cons b rest), otherwise
relativesqrt a0 eps n = relative eps (repeat (next n) a0)
```

and translates fairly directly to Clojure as:

```clojure
(defn next
  [n x]
  (/ (+ x
        (/ n 1.0 x))
     2.0))

(defn repeat
  [f a]
  (cons a (lazy-seq (repeat f (f a)))))

(defn relative
  [eps [a b & rs]]
  (if (>= eps (Math/abs (dec (/ a 1.0 b))))
    b
    (relative eps (cons b rs))))

(defn relativesqrt
  [a0 eps n]
  (relative eps (repeat #(next n %) a0)))
```

where `(cons hd (lazy-seq tl))` is how you build a lazy list in Clojure.

The main surprise in Section 4 did not come from any language issue, but rather
from the example computation chosen by Hughes. At the end of the section, he
presents:

```
super (integrate sin 0 4)
```

as an example of a quickly converging integral. The definition of `super` is
given as:

```
super s = map second (repeat improve s)
improve s = elimerror (order s) s
elimerror n (Cons a (Cons b rest)) =
  Cons ((b * (2^n) - a)/(2^n-1)) (elimerror n (Cons b rest))
order (Cons a (Cons b (Cons c rest))) = round (log2 ((a - c)/(b - c) - 1))
round x = x rounded to the nearest integer
log2 x = the logarithm of x to the base 2
```

and integrate is given as:

```
integrate f a b = integ f a b (f a) (f b)
integ f a b fa fb = Cons ((fa + fb) * (b - a) / 2)
                         map addpair (zip2 (integ f a m fa fm)
                                           (integ f m b fm fb))
where m = (a + b) / 2
      fm = f m
```

While translating that into Clojure poses no particular issue, the resulting
computation actually diverges almost immediately:

```
whyfp.core=> (take 10 (super (integrate2 #(Math/sin %) 0 4)))
(1.0617923583434352
 1.5780150025674877
 1.690241935320747
 ##-Inf
 ##NaN
 ##NaN
 ##NaN
 ##NaN
 ##NaN
 ##NaN)
```

I do not have a good explanation for why this happens. The other final example:

```
improve (integrate f 0 1)
where f x = 1/(1 + x * x)
```

does converge to \\(\pi/4\\) as claimed.

### AI & tree walking

The last (pre-conclusion) section of the paper is about extending the lazy list
approach from Section 4 to trees, with a typical "AI" game-tree-walking running
example. Provided we use a lazy implementation of `maptree` as previously
discussed, there is no new information in this section about how Clojure holds
up as a language supporting functional programming, nor indeed any real new
information about the advantages of functional programming.

This section was a lot more effort to write code for, because it builds upon
undefined ("left as an exercise") functions for generating the game tree being
walked.

### Before we conclude...

So far, Clojure seems to be holding up pretty well. The main issue I
encountered while trying to convert the code in this paper was around currying;
I personally do not believe that currying is a good idea to begin with, so I am
not bothered by that.

I am, however, a little bit bothered by the fact that all of the use-cases for
laziness in this paper boiled down to lazy lists. Lazy lists are an important
use-case, but they make a poor argument for making your entire language lazy,
as it is quite easy to integrate lazy lists in an otherwise eager language (as
Clojure demonstrates).

So before I conclude this post, I did want to show an example of laziness that,
as far as I can tell, is not easily expressed as a lazy list, and which I have
no idea how to express in Clojure. This example comes from the [advent of code
2019] set of problems. Without going too far into the details, by day 6 we have
built a small virtual machine interpreter (dubbed "IntMachine") that one could
model, as I did, as a function that looks like[^3]:

```haskell
execIntCode :: [Int] -> [Int] -> [Int]
```

Pretty straightforward, right? It's a function that takes two lists of
integers, one that represents whatever the machine is going to read and one
that represents the internal code of the machine, and it writes out some
output, also as a list of ints.

On day 7, we need to plug these machines together. This is straightforward
enough to do when you do it linearly. Here is a diagram taken from the [puzzle
explanation]:

```
    O-------O  O-------O  O-------O  O-------O  O-------O
0 ->| Amp A |->| Amp B |->| Amp C |->| Amp D |->| Amp E |-> (to thrusters)
    O-------O  O-------O  O-------O  O-------O  O-------O
```

This translates directly to code that will look similar in pretty much any
language (we define the function `run` as all five machines are running the
same "Amp" code):

```haskell
chain code [pa, pb, pc, pd, pe] =
  let run inputs = Lib.execIntcode inputs code
      a_out = run (pa:[0])
      b_out = run (pb:a_out)
      c_out = run (pc:b_out)
      d_out = run (pd:c_out)
      e_out = run (pe:d_out)
  in e_out
```

This is super-straightforward: run the first machine, collect its output, use
that as the input for the next machine. Right? So far, so good.

In the second part of that same problem, we need to add a feedback loop:
suddenly we _aslo_ want the output of the last machine to serve as additional
input to the first machine. Here is the diagram from the problem description:

```
      O-------O  O-------O  O-------O  O-------O  O-------O
0 -+->| Amp A |->| Amp B |->| Amp C |->| Amp D |->| Amp E |-.
   |  O-------O  O-------O  O-------O  O-------O  O-------O |
   |                                                        |
   '--------------------------------------------------------+
                                                            |
                                                            v
                                                     (to thrusters)
```

How do you handle that? None of my code thus far had been written with this
kind of use-case in mind, but because I had written it in a lazy language, the
solution was as simple as:

```haskell
feedback code [pa, pb, pc, pd, pe] =
  let run inputs = Lib.execIntcode inputs code
      a_out = run (pa:0:e_out)
      b_out = run (pb:a_out)
      c_out = run (pc:b_out)
      d_out = run (pd:c_out)
      e_out = run (pe:d_out)
  in last e_out
```

That's right. I can just plug the output of the fifth machine directly to the
input of the first machine. Just like that.

I find this use-case way more convincing than lazy list filtering, but maybe
that's just because I have been used to lazy lists in Clojure for a long time,
whereas this kind of full-language laziness is still fairly new to me.

### Conclusion

The [paper] was first written in 1989, at which point I was not actively
involved in software engineering. I imagine the argument for higher-order
functions, garbage collection[^4] and immutable data was not an easy one to
make. This is 2021, though, and hopefully by now everyone knows that
higher-order functions are good and that sections of code that are free of side
effects are much easier to work with, even in languages and programs where they
do not comprise the entirety of the code base.

This leads me to think that the main open question left here is that of
laziness. My personal opinion at this point is that laziness is just not worth
it in general. The IntMachine example above is great, but the cost is a much
degraded development experience on a daily basis, for the entire code base. And
I have only ever had that one example of useful non-list laziness.

The price of laziness today is that it makes code harder to debug[^5], and it
makes side effects harder to reason about. You may be thinking "hang on, isn't
the whole point of this paper that you should not have side effects?", and
you'd be right. But cute numerical computations aside, real code does have side
effects (even when you hide them in a monad) because it does need to actually
do something. Also, in this context I consider performance as a side effect,
and while laziness does not strictly speaking make performance worse[^6], it
does make it a lot harder to predict and reason about.

So, for now, I tend to prefer the Clojure approach, which yields most of the
benefits of laziness with few of its downsides. However, better tooling for
lazy languages could tip my opinion on this topic.

[^1]: I personally like this definition. I tend to think of it as imperative
  programming being ultimately based on the notion of a Turing machine, and
  functional programming being ultimately based on lambda calculus.
[^2]: By "good support" here I do not just mean that lazy lists exist: they are
  a very central abstraction in the standard library and most collection
  functions work lazily and return lazy lists, even when provided with non-lazy
  lists as inputs.
[^3]: This code is split between a [library with the IntMachine code] and the
  [solution for day 7].
[^4]: While garbage collection is not explicitly addressed in the paper,
  higher-order functions with immutable data are fairly hard to implement
  without it, not to mention laziness. I believe garbage collection is as
  essential to functional programming as functions themselves.
[^5]: Stack traces don't make much sense in a lazy context.
[^6]: In fact, one could argue that lazy semantics at the language level give a
  compiler more freedom to implement optimizations, and therefore could lead to
  better performance overall. Haskell performance in general is quite good for
  such a high-level language. My issue here is not raw performance but
  predictability.

[Why Why Functional Programming Matters Matters]: http://weblog.raganwald.com/2007/03/why-why-functional-programming-matters.html
[Reginald Braithwaite]: https://raganwald.com
[paper]: https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf
[Clojure]: https://clojure.org
[Haskell]: https://www.haskell.org
[core.match]: https://github.com/clojure/core.match
[advent of code 2019]: https://adventofcode.com/2019
[puzzle explanation]: https://adventofcode.com/2019/day/7
[here]: https://github.com/gaverhae/cuddly-octo-palm-tree/blob/5344d3d960f841955a96789608f4967cbc2df952/papers/whyfp/src/whyfp/core.clj
[solution for day 7]: https://github.com/gaverhae/cuddly-octo-palm-tree/blob/36c95606cd66379072ecad8f933c7cd1b8239e1c/advent_of_code/2019/adventofcode.com/src/Day7.hs
[library with the IntMachine code]: https://github.com/gaverhae/cuddly-octo-palm-tree/blob/36c95606cd66379072ecad8f933c7cd1b8239e1c/advent_of_code/2019/adventofcode.com/src/Lib.hs

