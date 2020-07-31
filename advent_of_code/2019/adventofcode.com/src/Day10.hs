module Day10
(
  parse,
  solution
) where

import Lib ((|>))
import Data.Ratio ((%))
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

parse :: String -> [(Int, Int)]
parse s = lines s
        |> zip [0..]
        |> map (\(y, line) -> line
                           |> zip [0..] 
                           |> filter ((== '#') . snd)
                           |> map fst
                           |> map (\x -> (x, y)))
        |> concat

count_angles :: [(Int, Int)] -> Int
count_angles vs =
  vs
  |> map (\(x, y) -> let g = gcd x y
                     in (signum x, signum y, quot x g, quot y g))
  |> Set.fromList
  |> Set.size

asteroids :: (Int, [(Int, Int)], (Int, Int))
asteroids =
  (30,
  [(-7,-3),
   (-2,-3),
   (-1,-3),
   (0,-3),
   (1,-3),
   (2,-3),
   (6,-3),
   (-8,-2),
   (-7,-2),
   (-3,-2),
   (-2,-2),
   (0,-2),
   (1,-2),
   (2,-2),
   (3,-2),
   (4,-2),
   (7,-2),
   (8,-2),
   (-8,-1),
   (-7,-1),
   (-3,-1),
   (1,-1),
   (3,-1),
   (4,-1),
   (5,-1),
   (6,-1),
   (7,-1),
   (-6,0),
   (4,0),
   (5,0),
   (6,0),
   (-6,1),
   (-4,1),
   (2,1),
   (7,1),
   (8,1)],
  (8,3))

solution :: String
solution =
  asteroids
  |> (\(_, m, o) -> m
                    |> map (\(x, y) -> let quadrant = case (x >= 0, y <= 0) of
                                                        (True, True) -> 0
                                                        (True, False) -> 1
                                                        (False, False) -> 2
                                                        (False, True) -> 3
                                           angle = if x == 0
                                                   then (maxBound % (signum y))
                                                   else (y % x)
                                           distance = x * x + y * y
                                       in (quadrant, angle, distance, x, y))
                    |> foldl (\m o@(q, a, d, x ,y) -> Map.insertWith (++) (q, a) [(d, x, y)] m)
                             Map.empty
                    |> Map.toList
                    |> List.sort
                    |> map (\(k, ls) -> List.sort ls)
                    |> List.transpose
                    |> concat
                    |> map (\(d, x, y) -> (x, y))
                    |> map (\(x, y) -> let (x0, y0) = o in 100 * (x + x0) + (y + y0))
                    |> map show
                    |> unlines)
