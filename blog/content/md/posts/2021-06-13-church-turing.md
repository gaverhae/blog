{:title "Church-Turing equivalence"
 :layout :post
 :tags ["paradigms"]}

In the [first post][turing] of this series, I presented the notion of a Turing
machine, which I argued is important because we can build physical
representations of them. In the [second post][lambda], I presented lambda
calculus, which I argued is important because it gives us the fundamental tools
to manage complexity (i.e. _abstraction_). In this post, I explain why I think
knowing about both of them is important to the practicing programmer.

[turing]: https://cuddly-octo-palm-tree.com/posts/2021-05-30-imperative-turing
[lambda]: https://cuddly-octo-palm-tree.com/posts/2021-06-06-functional-church

First, I will finally outline the equivalence proof I have alluded to in both
previous posts. Then, I'll expand on what that specific proof implies for
real-world programming languages, and finally what that means for practical
code writing.

### Equivalence proof

The formal proof of equivalence is way outside the scope of this blog. The
general principle behind the proof, however, has profound implications for the
working programmer, so here is a high-level, informal sketch of the proof:

```haskell
module Main where

import Control.Monad (ap, liftM)
import Data.Char (chr, ord)

data Exec a where
  Bind :: Exec a -> (a -> Exec b) -> Exec b
  Return :: a -> Exec a
  MoveRight :: Exec ()
  MoveLeft :: Exec ()
  Increment :: Exec ()
  Decrement :: Exec ()
  Input :: Exec ()
  Output :: Exec ()
  Get :: Exec Int

instance Functor Exec where fmap = liftM
instance Applicative Exec where pure = return; (<*>) = ap
instance Monad Exec where return = Return; (>>=) = Bind

parse :: String -> Exec ()
parse = \case
  ('>':xs) -> MoveRight >> parse xs
  ('<':xs) -> MoveLeft >> parse xs
  ('+':xs) -> Increment >> parse xs
  ('-':xs) -> Decrement >> parse xs
  ('.':xs) -> Output >> parse xs
  (',':xs) -> Input >> parse xs
  ('[':xs) -> let (till, after) = matching 0 ([],xs)
                  loop = do
                    v <- Get
                    if v == 0
                    then parse after
                    else do
                      parse till
                      loop
              in loop
  (_:xs) -> parse xs
  [] -> Return ()
  where
  matching :: Int -> (String, String) -> (String, String)
  matching 0 (till, ']':after) = (reverse till, after)
  matching n (till, ']':after) = matching (n - 1) (']':till, after)
  matching n (till, '[':after) = matching (n + 1) ('[':till, after)
  matching n (till, a:after) = matching n (a:till, after)
  matching n (till, []) = undefined

run :: String -> String -> String
run program input =
  toString $ exec (parse program) (0, [], fromString input) (\_ _ -> [])
  where
  toString :: [Int] -> String
  toString = map chr
  fromString :: String -> [Int]
  fromString = map ord
  exec :: Exec a -> (Int, [(Int, Int)], [Int]) -> (a -> (Int, [(Int, Int)], [Int]) -> [Int]) -> [Int]
  exec m state@(pointer, memory, input) cont = case m of
    Bind prev step -> exec prev state (\a state -> exec (step a) state cont)
    Return a -> cont a state
    MoveRight -> cont () (pointer + 1, memory, input)
    MoveLeft -> cont () (pointer - 1, memory, input)
    Increment -> cont () (pointer, update memory pointer (+1), input)
    Decrement -> cont () (pointer, update memory pointer (subtract 1), input)
    Get -> cont (get memory pointer) state
    Input -> cont () (pointer, update memory pointer (\_ -> (head input)), tail input)
    Output -> get memory pointer : cont () state
  update :: [(Int, Int)] -> Int -> (Int -> Int) -> [(Int, Int)]
  update [] p f = [(p, f 0)]
  update ((k,v):mem) p f = if k == p
                           then (k, f v) : mem
                           else (k, v) : update mem p f
  get :: [(Int, Int)] -> Int -> Int
  get [] p = 0
  get ((k,v):mem) p = if k == p
                      then v
                      else get mem p

main :: IO ()
main = do
  print $ run "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++." ""
```

What's going on here? Let's walk through each step in this proof:

1. This is Haskell code. Outside of `IO`, Haskell can be seen  as just another
   syntax for lambda calculus. Therefore, ignoring `main` (which in this case
   is just a test), this is a lambda term.
2. I can run Haskell code on my laptop, which means it ultimately runs on a
   Turing machine (my CPU). If my CPU, a universal Turing machine (UTM), can
   run arbitrary lambda terms (in the form of Haskell programs), it follows that a
   UTM can compute anything a lambda term can compute, and therefore Turing
   machines are at least as powerful as lambda calculus.
3. This specific Haskell program happens to be a slow, non-interactive, likely
   bug-ridden implementation of a [brainfuck] interpreter, which is the
   simplest UTM I could think of.
4. It follows that this lambda term is a UTM and therefore lambda calculus can
   compute anything a Turing machine can compute. Which means lambda calculus
   must be at least as powerful as Turing machines.
5. For \\(A \\geq B\\) and \\(B \\geq A\\) to both be true at the same time, we
   need to have \\(A = B\\) (where \\(A\\) is the expressive power of Turing
   machines and \\(B\\) is the expressive power of lambda calculus).

[brainfuck]: https://esolangs.org/wiki/Brainfuck

### There is no pure (and useful) language

Let's take a second look at the _structure_ of that proof. This is, in effect,
an _existence_ proof: we prove that lambda calculus can do anything a Turing
machine can do by exhibiting a specific lambda term (i.e. "program") that
simulates a universal Turing machine. Conversely, we prove that Turing machines
can do anything a lambda term can do by exhibiting a specific Turing machine
that can compute arbitrary lambda terms.

The practical implication here is that _either system can be embedded inside
the other_. More concretely, this means that it is possible to design a
programming language with features from both. And indeed, every major
programming language of today _is_ a mix of both.

Languages that tend more towards the Turing side usually still have a notion of
_expression_ (as opposed to _instructions_), which is semantically equivalent
to a \\(\\beta\\)-reduction. Very few languages do not have the notion of a
function definition, a.k.a. lambda calculus' _abstraction_.

Conversely, languages that tend more towards lambda calculus still need some
notion of writing something somewhere at some point, else they'd be completely
useless. This can be done with more or less ceremony, but "side-effects", as
functional programmers tend to call the Turing-machine bits of their languages,
are always possible.

In effect, no practically useful language can be purely one or the other: the
scale of programs we attempt to write these days _requires_ the power of
abstraction given by lambda calculus, while the necessity of running on
computers that exist, and of having our programs interact with the rest of the
world, similarly _requires_ the synchronous writing ability of a Turing machine.

More importantly than languages, any modern, useful _program_ will be written
using a mix of both functional and imperative programming, regardless of which
language was used. So what's the point of all this theory if we're ultimately
forced to use both? Glad you asked. Let's start with the pollution problem that
arises when mixing functional and imperative code.

### The pollution problem

It may not yet be entirely obvious why Turing machines are necessary (at the
"writing programs" level). If we can have a compiler from lambda terms to CPU
instructions, why could we not stick with lambda terms? The answer to that is
time. Not in the sense of efficiency, but in the sense of coordination. Why do
we need coordination, and what do we need to coordinate with?

A computer program is not just an intellectual exercise, and a CPU does not
work in isolation. As a programmer, you want things to appear on the screen,
you want network packets to be sent, you want data to be written on disks.
Because the CPU is ultimately just a Turing machine, all of those things have
been reduced to the single mechanism available to a Turing machine: writing
symbols on a ribbon. A modern computer is _more_ than a Turing machine in the
sense that it is more than just a CPU. It still cannot compute anything a
Turing machine could not, but it can make external things happen by writing on
specific parts of the ribbon. Conceptually, the ribbon (memory) is shared
between the Turing machine (CPU) and a number of other things that read from
(and sometimes write to) the same ribbon.

This is where we need coordination: all these processes reading from and
writing to the same ribbon need to not step on each other's toes. Our Turing
machine needs some notion of time (each step of evaluation in the Turing
machine model) so that its own actions can be interleaved safely with what the
other components are doing.

Lambda calculus does not have such a neat notion of time, or indeed a concept
of writing something on a ribbon. We could give up and stick to Turing
machines. They're the ones we can build, they're the ones that let us interact
with the rest of the world. But if we want to write reasonably complex
programs, being stuck with brainfuck is not exactly a great position to be in.

We need lambda calculus for its ability to _abstract_, as that is the primary
tool we have for managing complexity, and managing complexity is what "software
engineering" is all about. But our programs always have to be a Turing machine
at the "top" level, as the ultimate driver. There's a problem with that. If
we're simulating lambda calculus within a Turing machine (as we almost always
are), it is possible to design a programming language in such a way that you
can reach back out to your Turing machine's ribbon from what would otherwise
look like a lambda term. In fact, the vast majority of languages allow exactly
that. If you do it, though, you break the lambda term abstraction by making
\\(\\beta\\)-reductions invalid, and you essentially lose all of the properties
that made you want lambda calculus in the first place.

You can only reap the full benefits of abstraction on expressions that are real
lambda terms, otherwise known as "purely functional" or "referentially
transparent". If the call stack of an expression contains any callback to the
encapsulating Turing machine, you're out of luck.

And if we want to have the full power of abstraction in as much of our code as
possible, it follows that the best approach, as we have to _start our programs
from a Turing machine_, is to keep all of the side-effects as high in the call
stack as possible and "drop down" to pure lambda terms as early (and as often)
as possible. Which brings us to Haskell.

### So what is functional programming?

In light of all the context I have built up over this and the
[previous][turing] [two][lambda] posts, I feel I can now give a useful,
practical definition for "functional programming":

> **Functional programming** is a set of tools and techniques that aim at
> maximizing the proportion of a program that consists of pure lambda terms.

Languages can be compared on many different axes, but if we want to focus on
the imperative v. functional one, we need to look at what they make easy and
what they make annoying. In that light, a _functional programming language_ is
one that encourages, through a combination of syntax, language features,
standard library, default namespace, ecosystem and community, the use of
functional programming techniques and the maximization of lambda terms.

Haskell is the most extreme example I know of, so let's take a look at it. It
should be obvious at this point that I'm not a fan of the language's claim as
being "purely functional"; like any other useful language, Haskell code can
produce and depend on side-effects. No Haskell program is purely functional.

What Haskell does have is a type system that keeps track of side-effects.
Haskell code starts from the `IO` monad, which is really a Turing machine
disguised as a lambda term. From there, you can drop down to non-`IO`, a.k.a.
"pure", code, and the type system will ensure that, once you've declared an
expression as pure, there is no way for anything in the call stack of that
expression to ever get back to the enclosing `IO`[^IO], meaning you (and the
compiler!) can use the full power of lambda calculus to reason about, simplify
and optimize such expressions.

[^IO]: There are, of course, ways to work around that. No design-by-committee
  system can survive in the real world for any length of time without evolving
  backdoors. By-and-large, though, actual use of those backdoors is extremely
  rare, and generally frowned-upon by the community, which means that one can
  mostly pretend they don't exist.

In effect, Haskell forces you to have a very clean limit between the Turing
machine you start from, coordinating all of the interactions with the rest of
the system, and all of the little islands of lambda calculus that will happen
below that. I like to think of that line as the Turing/lambda frontier.

Haskell is by no means a perfect language, but it _is_ extremely good at
nudging programmers in the right direction on this aspect of program design.
It's important to realize, though, that this approach has the same benefits in
almost any language, and you definitely do not need to be writing Haskell to
use it. Some languages will let you push the Turing/lambda frontier higher than
others, but striving to push it as high as possible is always a good idea.
