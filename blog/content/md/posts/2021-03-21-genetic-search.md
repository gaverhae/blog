{:title "Genetic algorithms"
 :layout :post
 :tags ["ai" "haskell"]}

A couple days ago I stumbled upon [this presentation][pres] about using AI to
tweak the JVM, which I found really interesting.

The presentation describes how they use "AI" to automatically figure out the
best set of JVM tuning flags for a given application. If you're interested in
JVM tuning, whether you know anything about it or not, it's a good presentation
to watch. The gist of it is that modern JVMs have upwards of 800 different
flags you can set, and so far setting them has been a bit of a dark art, with
lots of folk wisdom and intuitions. The talk specifically walks through four
well-known truths about JVM tuning and shows example cases where they did not
hold.

What I found interesting about the video is not really the content of the
presentation itself, but the simple, obvious-in-hindsight idea of looking at
JVM tuning as a search problem. A search problem is characterized by three
conditions:

- There is a fairly well-defined "solution space", in which any point is a
  potential solution. In other words, generating a random potential solution is
  easy. Enumerating all possible solutions is also (usually) easy, but we're of
  course only interested in cases where it would take a long time, because
  otherwise we could just try all of them.
- It is fairly easy to evaluate a specific solution. This is usually phrased as
  having a so-called "objective" function that, given a potential solution,
  returns a (scalar) score. The _objective_ of the search is to optimize
  (usually minimize) that score.
- It is not obvious how to determine the best solution without trying all of
  them.

One could recognize traces of NP-completeness in that description, though this
is a bit broader and of course a lot less formal. Search algorithms have been
used with reasonable success to address NP-complete problems such as the
travelling salesman, assuming you define success as getting a solution "close
enough" to optimal, rather than the optimal one.

This presentation made we wonder if there are any other "obvious" search
problems I am currently not recognizing as such.

When I do recognize I'm faced with a search problem, I usually turn to genetic
search, also known as genetic algorithm or evolutionary algorithm, because it
usually works fairly well and it is super easy to remember and implement in
almost any language without the need for specialized libraries. Most other AI
techniques require adding dependencies (always a risk) and therefore reading
tons of documentation to figure out how to plug your problem into someone
else's idea of how the API should look like. Or maybe you have another search
algorithm you just know and can implement in your sleep; for whatever reason,
genetic search just clicks with my brain.

Genetic search is based on what is arguably the most powerful idea in all of
science[^1]: evolution through natural selection. Going back to the basics, to
get evolution going we need:

- Individuals that can die and reproduce. If they could not die, we could not
  make progress[^2]; if they could not reproduce before they die, we would have
  to stop fairly early. These will be our candidate solutions, and for
  simplicity we'll consider synchronous generations, i.e. all births and deaths
  happen at the same time.
- An environment in which some individuals fare better than others. In our case
  this will be the objective function, which in this context we usually call
  the "fitness" function (how well individuals fit their environment).
- Selection pressure: when building the new generation, individuals with a
  better fitness should have a better chance of surviving as well as a better
  chance of producing offspring.
- Random mutations: if the offspring are exact copies of their parents, there
  is no way to improve and we won't explore much of the search space.
- Sex: the evolution of life on Earth has shown that sexual reproduction
  introduces a much wider variety than just random mutations alone, so adding
  some sort of interbreeding between (successful) solutions will allow us to
  explore more of the search space faster.

With these principles in mind, we can start building out a genetic search
algorithm. Let's start at the top with the `genetic_search` function. What
should its type be? Based on the above, it needs:

- A way to turn a potential solution into a number. We'll make a choice here
  and say that the score is a real, positive number, and that lower is better.
  We can think of it as a cost, or a duration, that we're trying to minimize. So
  we need an argument `fit` of type `solution -> Double`.
- A way to apply random mutations to a solution. This will be a function
  `mutate` of type `solution -> solution`.
- A way to combine two solutions to produce a new solution, i.e. "sex". This is usually
  called _crossover_ in the literature so we'll go with that: an argument
  `crossover` of type `solution -> solution -> solution`.
- A random population to start with.

So at first glance it may seem like this is what we're looking for:

```haskell
genetic_search :: (solution -> Double)
               -> (solution -> solution)
               -> (solution -> solution -> solution)
               -> [solution]
               -> solution
genetic_search fit mutate crossover init = undefined
```

However, a lot of these operations require some sort of source of randomness,
so we need to get that from somewhere. We have a couple options here. First, we
could just use a language with unconstrained side-effects. I've used Haskell
notation above to describe the type, but just because we're thinking about
types doesn't mean we have to write in Haskell. Most other languages have some
sort of standard `random` function that returns a random number. We probably
don't need cryptographically secure random numbers here, so default language
libraries should be fine.

What if we do want to use Haskell, though? As I'm writing this, it looks like
the state of random numbers in Haskell is in a bit of a mess, with the [current
Stack snapshot] providing a fairly old version of `random` (1.1, apparently
from September 2014) despite a newer version being available (1.2, published in
2020), while "the internet" is telling me to look at `Crypto.Random` instead
and there are [three] [different] [packages] providing that namespace.

Let's ignore all that and just assume we receive a list of random values. Let
the caller figure out how to generate them, and how much effort they want to
put into making it cryptographic randomness (or not). More precisely, we'll be
taking in a `[Double]` where the elements are assumed to be between 0 and 1.

This does mean that we need to track how many of these values we consume in
each function call. It would be a bit tedious for each function to take in the
list and have to return its unconsumed tail, so instead we'll define a monad to
keep track of that for us. We really only need one operation here, "get a
random number", so here is our monad's definition:

```haskell
{-# LANGUAGE GADTs #-}
import qualified Control.Monad

{- ... -}

instance Functor WithRandom where fmap = Control.Monad.liftM
instance Applicative WithRandom where
  pure = return
  (<*>) = Control.Monad.ap
instance Monad WithRandom where return = Return; (>>=) = Bind

data WithRandom a where
  Bind :: WithRandom a -> (a -> WithRandom b) -> WithRandom b
  Return :: a -> WithRandom a
  GetRand :: WithRandom Double

exec_random :: WithRandom a -> [Double] -> ([Double] -> a -> b) -> b
exec_random m s cont = case m of
  Bind ma f -> exec_random ma s (\s a -> exec_random (f a) s cont)
  Return a -> cont s a
  GetRand -> cont (tail s) (head s)
```

There is one more thing we have not really discussed yet: when should we stop?
Once again we'll make that our caller's problem by returning a (lazy) list
where each element is the best individual of its generation.

Our updated signature would look something like:

```haskell
genetic_search :: (solution -> Double)
               -> (solution -> WithRandom solution)
               -> (solution -> solution -> WithRandom solution)
               -> [solution]
               -> [Double]
               -> [(solution, Double)]
```

There are a few extra paramaters we could want to pass to this function, but
for now we'll hardcode them. Because they would have to be set partially based
on the length of the initial list (size of the initial generation), we'll
replace the fourth argument with a function to generate random individuals
rather than an initial list, so we can generate an initial population of the
appropriate size.

Because we want to return a lazy list, we have to create it outside of the
monad.[^sequencing] This means we will need to do a little bit of manual state
(list of random `Double`) threading.

[^sequencing]: Monads are rarely used _just_ for sequencing, but sequencing
code is their core, basic feature. That's literally what `bind` (`>>=`) does.
This makes it a bit tricky to return a lazy list from a monad. Imagine you have
a `loop` function that you want to return a lazy list. For simplicity, let's
assume it's an iterable function (i.e. if it weren't in a monad, you'd call
`iterate loop init`). You can get the next element of your lazy list by calling
`hd <- loop prev`. But then, how do you proceed? If you try to write `tl <-
loop hd` so that you can `return $ hd:tl`, `tl` needs to be computed and you
lose lazines. If you don't want to compute it, you could write `let tl = loop
next`, but then how do you combine it with `hd`?

Let's start with creating the initial generation, and thread the remaining
randomness through the main loop. Manual state threading is a bit annoying in
the general case, but there is one code pattern in which it's fairly natural:
[continuation-passing style][cps]. And it just so happens that our monadic
`exec_random` function expects a continuation. Happy accident, I guess.

```haskell
{- LANGUAGE ScopedTypeVariables -}

{- ... -}

genetic_search :: forall solution.
                  Eq solution
               => (solution -> Double)
               -> (solution -> WithRandom solution)
               -> (solution -> solution -> WithRandom solution)
               -> WithRandom solution
               -> [Double]
               -> [(solution, Double)]
genetic_search fitness mutate crossover make_solution rnd =
  map head $ exec_random init
                         rnd
                         (\rnd prev -> loop prev rnd)
  where
```

We enable `ScopedTypeVariables` to make it a bit easier to write type
signatures for nested functions, so that `solution` refers to the same one type
throughout the entire definition of `genetic_search`.

Let's define the `loop` function. What we want it to do is create a lazy list.
As mentioned, creating a lazy list from within monadic code is a bit tricky, so
instead we'll just assume we have a function `step` that produces the next
generation based on the previous one, and the only thing `loop` needs to do is
to repeatedly call this `step` function within the monad. With `loop` itself
outside the monad, it is trivial to maintain laziness. Again, we turn to
continuations, and the structure of `loop` is basically the same as that first
line we just wrote:

```haskell
  loop :: [(solution, Double)] -> [Double] -> [[(solution, Double)]]
  loop prev rnd = prev : exec_random (step prev)
                                     rnd
                                     (\rnd next -> loop next rnd)
```

We now need to write `step` and `init`. Let's start with `init`. We already
have a recipe for creating a potential solution, so all we need to
do is call that repeatedly until we have the generation size we want. This
could be a parameter, but for the sake of exposition here we'll hardcode the
population size at 100.

We'll also need to compute the fitness of each solution. In most search
problems, computing the fitness is a rather expensive operation, so we want to
do it only once per solution. To achieve that, we'll work internally with
tuples `(solution, Double)` where the second element is the fitness. It's also
going to be useful if we maintain the invariant that a generation is a _sorted_
list of such tuples such that the fitter ones are at the front.

```haskell
import qualified Data.Sort

{- ... -}

  rep :: Int -> WithRandom a -> WithRandom [a]
  rep n f = Control.Monad.forM [1..n] (\_ -> f)
  fit :: solution -> (solution, Double)
  fit s = (s, fitness s)
  srt :: [(solution, Double)] -> [(solution, Double)]
  srt = Data.Sort.sortOn snd
  init :: WithRandom [(solution, Double)]
  init = srt <$> map fit <$> rep 100 make_solution
```

We now turn to `step`, which is where all the work happens. Following our
guiding principles from before, `step` needs to select some survivors and make
up some children. There are many ways to select survivors; for now we'll keep
it simple and keep the ten most fit, as well as the three least fit. We keep
some of the "bad" ones in the hope that they will help us explore more of the
solution space and prevent us from getting stuck in a local optimum.

```haskell
import qualified Data.List

{- ... -}

  step :: [(solution, Double)] -> WithRandom [(solution, Double)]
  step prev = do
    let survivors = take 10 prev ++ take 3 (reverse prev)
    children <- rep 87 (do
      parent1 <- carousel prev
      parent2 <- carousel (parent1 `Data.List.delete` prev)
      child <- crossover (fst parent1) (fst parent2)
      fit <$> mutate child)
    return $ srt $ survivors <> children
```

We're just missing one function: `carousel`. This is a function that will
randomly pick individuals from the given list based on their fitness, where the
most fit have the most chances of being picked. If we had increasing fitnesses
(i.e. bigger is better), we could easily do that by summing all the fitnesses
and generating a random number in that range. For simplicity, we'll ignore the
risk of zero fitness and just invert the score to get increasing values.

```haskell
  carousel :: [(solution, Double)] -> WithRandom (solution, Double)
  carousel gen = do
    let inverted = map (\(s, f) -> (s, 1  / f)) gen
    let total = foldl (+) 0 $ map snd inverted
    roll <- (* total) <$> GetRand
    let find ((s, f):tl) t = if t <= f then (s, 1 / f) else find tl (t - f)
        find _ _ = undefined -- shouldn't happen
    return $ find inverted roll
```

Now, what can we use this for? It should be able to help with any search
problem. As a simple example, let's see if it can find a reasonable solution
for this simple equation[^equation google]:

\\[z = 2 x\^2 + y\^2 + 1\\]

It should be pretty obvious for a human that the minimum of this curve is going
to be at \\(z = 1\\) for \\((x, y) = (0, 0)\\), since neither \\(x\^2\\) nor
\\(y\^2\\) can be negative, but our little genetic algorithm here has no
knowledge of mathematics. Let's see how it fares.

Our fitness function will be the paraboloid itself; an individual will be a
couple of coordinates (\\(x\\) and \\(y\\)):

```haskell
  let fitness (x, y) = 2 * (x ** 2) + (y ** 2) + 1
```

The mutation function will have a ten percent chance of changing each
coordinate by \\([-0.5, 0.5]\\):

```haskell
  let mutate (x, y) = do
      change_x <- GetRand
      dx <- GetRand
      change_y <- GetRand
      dy <- GetRand
      let new_x = if change_x < 0.1 then x + dx - 0.5 else x
      let new_y = if change_y < 0.1 then y + dy - 0.5 else y
      return (new_x, new_y)
```

The crossover function will take a simple mean of its parents most of the time,
but sometimes it will just take a coordinate straight from one of the parents
instead:

```haskell
  let crossover (x1, y1) (x2, y2) = do
      roll_x <- GetRand
      roll_y <- GetRand
      let mean_x = (x1 + x2) / 2
      let mean_y = (y1 + y2) / 2
      return (if roll_x < 0.05 then x1
              else if roll_x > 0.95 then x2
              else mean_x,
              if roll_y < 0.05 then y1
              else if roll_y > 0.95 then y2
              else mean_y)
```

The final piece of our modelisation is to generate random solutions to start
with. In this case I know that the minimum of that curve is somewhere in \\(x
\in [0, 10], y \in [0, 10]\\), so we'll generate our initial solutions in
that square:

```haskell
  let mk_sol = do
      rand_x <- GetRand
      rand_y <- GetRand
      return (rand_x * 10, rand_y * 10)
```

Now we're back to the problem of generating a list of random values. Using the
version of `random` I have easy access to (1.1), I can generate such a list
with something like:

```haskell
import qualified System.Random

{- ... -}

  let rng = System.Random.mkStdGen 0
  let rands = tail
              $ map fst
              $ iterate (\(_, rng) ->
                          System.Random.randomR (0::Double, 1) rng)
                        (0, rng)
```

Here is the complete code for reference (also on [github]).

```haskell
module Main where

import qualified Control.Monad
import qualified Data.List
import qualified Data.Sort
import qualified System.Random

instance Functor WithRandom where fmap = Control.Monad.liftM
instance Applicative WithRandom where
  pure = return
  (<*>) = Control.Monad.ap
instance Monad WithRandom where return = Return; (>>=) = Bind

data WithRandom a where
  Bind :: WithRandom a -> (a -> WithRandom b) -> WithRandom b
  Return :: a -> WithRandom a
  GetRand :: WithRandom Double

exec_random :: WithRandom a -> [Double] -> ([Double] -> a -> b) -> b
exec_random m s cont = case m of
  Bind ma f -> exec_random ma s (\s a -> exec_random (f a) s cont)
  Return a -> cont s a
  GetRand -> cont (tail s) (head s)

genetic_search :: forall solution.
                  Eq solution
               => (solution -> Double)
               -> (solution -> WithRandom solution)
               -> (solution -> solution -> WithRandom solution)
               -> WithRandom solution
               -> [Double]
               -> [(solution, Double)]
genetic_search fitness mutate crossover make_solution rnd =
  map head $ exec_random init
                         rnd
                         (\rnd prev -> loop prev rnd)
  where
  loop :: [(solution, Double)] -> [Double] -> [[(solution, Double)]]
  loop prev rnd = prev : exec_random (step prev)
                                     rnd
                                     (\rnd next -> loop next rnd)
  rep :: Int -> WithRandom a -> WithRandom [a]
  rep n f = Control.Monad.forM [1..n] (\_ -> f)
  fit :: solution -> (solution, Double)
  fit s = (s, fitness s)
  srt :: [(solution, Double)] -> [(solution, Double)]
  srt = Data.Sort.sortOn snd
  init :: WithRandom [(solution, Double)]
  init = srt <$> map fit <$> rep 100 make_solution
  step :: [(solution, Double)] -> WithRandom [(solution, Double)]
  step prev = do
    let survivors = take 10 prev ++ take 3 (reverse prev)
    children <- rep 87 (do
      parent1 <- carousel prev
      parent2 <- carousel (parent1 `Data.List.delete` prev)
      child <- crossover (fst parent1) (fst parent2)
      fit <$> mutate child)
    return $ srt $ survivors <> children
  carousel :: [(solution, Double)] -> WithRandom (solution, Double)
  carousel gen = do
    let inverted = map (\(s, f) -> (s, 1  / f)) gen
    let total = foldl (+) 0 $ map snd inverted
    roll <- (* total) <$> GetRand
    let find ((s, f):tl) t = if t <= f then (s, 1 / f) else find tl (t - f)
        find _ _ = undefined -- shouldn't happen
    return $ find inverted roll

main :: IO ()
main = do
  let fitness (x, y) = 2 * (x ** 2) + (y ** 2) + 1
  let mutate (x, y) = do
      change_x <- GetRand
      dx <- GetRand
      change_y <- GetRand
      dy <- GetRand
      let new_x = if change_x < 0.1 then x + dx - 0.5 else x
      let new_y = if change_y < 0.1 then y + dy - 0.5 else y
      return (new_x, new_y)
  let crossover (x1, y1) (x2, y2) = do
      roll_x <- GetRand
      roll_y <- GetRand
      let mean_x = (x1 + x2) / 2
      let mean_y = (y1 + y2) / 2
      return (if roll_x < 0.05 then x1
              else if roll_x > 0.95 then x2
              else mean_x,
              if roll_y < 0.05 then y1
              else if roll_y > 0.95 then y2
              else mean_y)
  let mk_sol = do
      rand_x <- GetRand
      rand_y <- GetRand
      return (rand_x * 10, rand_y * 10)
  let rng = System.Random.mkStdGen 0
  let rands = tail
              $ map fst
              $ iterate (\(_, rng) ->
                          System.Random.randomR (0::Double, 1) rng)
                        (0, rng)
  print $ map snd
        $ take 40
        $ genetic_search fitness mutate crossover mk_sol rands
```

Running this, we get:

```haskell
[3.5982987289689534,
 3.474893005959019,
 3.474893005959019,
 3.4737428040201532,
 3.150409327404966,
 2.971337534579598,
 2.3206068810142573,
 2.118654417921306,
 2.118654417921306,
 1.8936180968443044,
 1.2981059153760273,
 1.2981059153760273,
 1.2981059153760273,
 1.2981059153760273,
 1.2981059153760273,
 1.289376637990746,
 1.289376637990746,
 1.2062456035697653,
 1.2062456035697653,
 1.0913968802823735,
 1.0913968802823735,
 1.0913968802823735,
 1.0381828822576198,
 1.0381828822576198,
 1.0381828822576198,
 1.0381828822576198,
 1.0044503123987698,
 1.0044503123987698,
 1.0044503123987698,
 1.0044503123987698,
 1.0005674723723683,
 1.0005674723723683,
 1.0005674723723683,
 1.0005178583662893,
 1.0005178583662893,
 1.0000873800970638,
 1.0000873800970638,
 1.0000873800970638,
 1.0000873800970638,
 1.0000873800970638]
```

which confirms that the general principles work. This is of course a very easy
problem in that the entire curve has a single optimum (and is continuous and
therefore convex). This test is really just about making sure the basics of the
algorithm are correctly wired, so to speak.

You may have noticed that there are still quite a few parameters we could
tweak. For example:

- The size of a generation.
- The way in which we select survivors (and how many we pick).
- Whether non-survivors are allowed to make offspring.
- How we select parents.

This is in addition to all the choices we can already make with the above code:
the shape of a solution, how to mutate, how to crossover, and what the initial
set looks like.

With so many parameters to choose from, tweaking a genetic algorithm for a
particular problem is a bit of a dark art, with lots of folk wisdom and
intuitions.

[^1]: Most people think of evolution as a biological phenomenon, but it is much
  broader than that. It's really the governing principle for the change over
  time of any complex system.
[^2]: At this point it's best to think of "progress" as a very vague, general
  notion of "movement", i.e. the opposite of stagnation. This is not a
  _directed_ movement; there is no intention behind it.
[^equation google]: I have just discovered that if you type `z = 2 * x ^ 2 + y
  ^ 2 + 1` in Google you actually get a graph of the shape that describes. I
  think that's pretty cool.

[pres]: https://www.youtube.com/watch?v=9VvaxATyYsA
[current Stack snapshot]: https://www.stackage.org/lts-17.6
[three]: https://www.stackage.org/haddock/lts-17.6/cryptonite-0.27/Crypto-Random.html
[different]: https://www.stackage.org/haddock/lts-17.6/crypto-api-0.13.3/Crypto-Random.html
[packages]: https://www.stackage.org/haddock/lts-17.6/crypto-random-0.0.9/Crypto-Random.html
[cps]: https://en.wikipedia.org/wiki/Continuation-passing_style
[github]: https://github.com/gaverhae/cuddly-octo-palm-tree/tree/13053e8b7ae8d62c855dc6753b6603ffa8506e2c/dev/genetic
