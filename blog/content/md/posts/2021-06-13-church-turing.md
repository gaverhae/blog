{:title "Church-Turing equivalence"
 :layout :post
 :tags ["paradigms"]}

In the [first post][turing] of this series, I presented the notion of a Turing
machine, which I argued is important because we can build physical
representations of them. In the [second post][lambda], I presented lambda
calculus, which I argued is important because it gives us the fundamental tools
to manage complexity (i.e. _abstraction_).In this post, I will make a case for
why you should care as a practicing programmer.

[turing]: https://cuddly-octo-palm-tree.com/posts/2021-05-30-imperative-turing
[lambda]: https://cuddly-octo-palm-tree.com/posts/2021-06-06-functional-church

First, I will finally outline the equivalence proof I have alluded to in both
previous posts. Then, I'll expand on what that specific proof implies for
real-world programming.

## Equivalence proof

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
   syntax for lambda calculus. Therefore, ignoring `main`, this is a lambda
   term.
2. I can run Haskell code on my laptop, which means it ultimately runs on a
   Turing machine (my CPU). If my CPU can run arbitrary lambda terms, it
   follows that a universal Turing machine can compute anything a lambda term can
   compute, and therefore Turing machines are at least as powerful as lambda
   calculus.
3. This specific Haskell program happens to be a slow, non-interactive, likely
   bug-ridden implementation of a [brainfuck] interpreter, which is the
   simplest universal Turing machine I could think of.
4. It follows that this lambda term is a universal Turing machine, and
   therefore lambda calculus can compute anything a Turing machine can
   computer. Which means lambda calculus must be at least as powerful as Turing
   machines.
5. For \\(A \\geq B\\) and \\(B \\geq A\\) to both be true at the same time, we
   need to have \\(A = B\\) (where \\(A\\) is the expressive power of Turing
   machines and \\(B\\) is the expressive power of lambda calculus).

[brainfuck]: https://esolangs.org/wiki/Brainfuck

## No language is perfect

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

## What should I say now?

