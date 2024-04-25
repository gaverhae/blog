module Main where

import qualified Data.Sort
import qualified System.Random

data Rng = Rng [Double]

rnd :: Rng -> (Rng, Double)
rnd (Rng ds) = (Rng (tail ds), head ds)

genetic_search :: forall solution.
                  Eq solution
               => (solution -> Double)
               -> (Rng -> solution -> (Rng, solution))
               -> (Rng -> solution -> solution -> (Rng, solution))
               -> (Rng -> (Rng, solution))
               -> Rng
               -> [(solution, Double)]
genetic_search fitness mutate crossover make_solution rng_0 =
  let (rng_1, gen_0) = rep rng_0 100 make_solution
      gen_0' = srt $ map fit gen_0
  in map head (gen_0' : loop rng_1 gen_0')
  where
  rep :: Rng -> Int -> (Rng -> (Rng, a)) -> (Rng, [a])
  rep rng_0 0 f = (rng_0, [])
  rep rng_0 n f = let (rng_1, ss) = rep rng_0 (n - 1) f
                      (rng_2, s) = f rng_1
                  in (rng_2, s:ss)
  fit :: solution -> (solution, Double)
  fit s = (s, fitness s)
  srt :: [(solution, Double)] -> [(solution, Double)]
  srt = Data.Sort.sortOn snd
  loop :: Rng -> [(solution, Double)] -> [[(solution, Double)]]
  loop rng_0 prev =
    let (rng_1, next) = step rng_0 prev
    in next : loop rng_1 next
  step :: Rng -> [(solution, Double)] -> (Rng, [(solution, Double)])
  step rng_0 prev =
    let survivors = take 10 prev ++ take 3 (reverse prev)
        (rng_1, children) = rep rng_0 87 (\rng_0 ->
          let (rng_1, parent1) = carousel rng_0 prev
              (rng_2, parent2) = carousel rng_1 prev
              (rng_3, child) = crossover rng_2 (fst parent1) (fst parent2)
              (rng_4, child') = mutate rng_3 child
          in (rng_4, fit child'))
        in (rng_1, srt (survivors <> children))
  carousel :: Rng -> [(solution, Double)] -> (Rng, (solution, Double))
  carousel rng_0 gen =
    let inverted = map (\(s, f) -> (s, 1  / f)) gen
        total = foldl (+) 0 $ map snd inverted
        (rng_1, roll) = rnd rng_0
        roll' = total * roll
    in (rng_1, find inverted roll')
  find ((s, f):tl) t = if t <= f then (s, 1 / f) else find tl (t - f)
  find _ _ = undefined -- shouldn't happen

make_solution :: Rng -> (Rng, (Double, Double))
make_solution rng_0 =
  let (rng_1, rand_x) = rnd rng_0
      (rng_2, rand_y) = rnd rng_1
  in (rng_2, (rand_x * 10, rand_y * 10))

fitness :: (Double, Double) -> Double
fitness (x, y) = 2 * (x ** 2) + (y ** 2) + 1

mutate :: Rng -> (Double, Double) -> (Rng, (Double, Double))
mutate rng_0 (x, y) =
  let (rng_1, change_x) = rnd rng_0
      (rng_2, dx) = rnd rng_1
      (rng_3, change_y) = rnd rng_2
      (rng_4, dy) = rnd rng_3
      new_x = if change_x < 0.1 then x + dx - 0.5 else x
      new_y = if change_y < 0.1 then y + dy - 0.5 else y
  in (rng_4, (new_x, new_y))

crossover :: Rng -> (Double, Double) -> (Double, Double) -> (Rng, (Double, Double))
crossover rng_0 (x1, y1) (x2, y2) =
  let (rng_1, roll_x) = rnd rng_0
      (rng_2, roll_y) = rnd rng_1
      mean_x = (x1 + x2) / 2
      mean_y = (y1 + y2) / 2
  in (rng_2, (if roll_x < 0.05 then x1
              else if roll_x > 0.95 then x2
              else mean_x,
              if roll_y < 0.05 then y1
              else if roll_y > 0.95 then y2
              else mean_y))

main :: IO ()
main = do
  let rng = System.Random.mkStdGen 0
  let rands :: [Double]
      rands = tail
              $ map fst
              $ iterate (\(_, rng) ->
                          System.Random.randomR (0::Double, 1) rng)
                        (0, rng)
  print $ take 40 $ genetic_search fitness mutate crossover make_solution (Rng rands)
