module Main where

import qualified Control.Monad
import qualified Data.List
import qualified Data.Sort
import qualified System.Random

data OnDemand a b
  = Halt
  | Out b (OnDemand a b)
  | In (a -> OnDemand a b)

-- BEGIN exercise, unrelated

filterI :: (a -> Bool) -> OnDemand a a
filterI f =
  In (\a -> case f a of
    True -> Out a (filterI f)
    False -> (filterI f))

mapI :: (a -> b) -> OnDemand a b
mapI f = In (\a -> Out (f a) (mapI f))

compI :: OnDemand a b -> OnDemand b c -> OnDemand a c
compI i1 i2 =
  case i2 of
    Halt -> Halt
    Out c i2 -> Out c (compI i1 i2)
    In f -> case i1 of
      Halt -> Halt
      Out b i1 -> compI i1 (f b)
      In g -> In (\a -> compI (g a) i2)

-- END

instance Functor WithRandom where fmap = Control.Monad.liftM
instance Applicative WithRandom where
  pure = return
  (<*>) = Control.Monad.ap
instance Monad WithRandom where return = Return; (>>=) = Bind

data WithRandom a where
  Bind :: WithRandom a -> (a -> WithRandom b) -> WithRandom b
  Return :: a -> WithRandom a
  GetRand :: WithRandom Double

exec_random :: WithRandom a -> (a -> OnDemand Double b) -> OnDemand Double b
exec_random m cont = case m of
  Bind ma f -> exec_random ma (\a -> exec_random (f a) cont)
  Return a -> cont a
  GetRand -> In (\r -> cont r)

genetic_search :: forall solution.
                  Eq solution
               => (solution -> Double)
               -> (solution -> WithRandom solution)
               -> (solution -> solution -> WithRandom solution)
               -> WithRandom solution
               -> OnDemand Double (solution, Double)
genetic_search fitness mutate crossover make_solution =
  exec_random init (\prev -> Out (head prev) (loop prev))
  where
  loop :: [(solution, Double)] -> OnDemand Double (solution, Double)
  loop prev = exec_random (step prev) (\next -> Out (head next) (loop next))
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
  loop rands 40 $ genetic_search fitness mutate crossover mk_sol
  where
  loop :: [Double] -> Int -> OnDemand Double ((Double, Double), Double) -> IO ()
  loop rs n od =
    if n == 0
    then return ()
    else case od of
      Halt -> return ()
      In f -> do
        next_rand <- pure (head rs)
        loop (tail rs) n (f next_rand)
      Out v k -> do
        print v
        loop rs (n - 1) k
