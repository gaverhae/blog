module Day10
(
  parse,
  solution
) where

import Lib ((|>))
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

kill_order :: [(Int, Int)] -> [(Int, Int)]
kill_order asteroids =
  asteroids
  |> map (\(x, y) -> let g = gcd x y
                         quadrant = case (x >= 0, y <= 0) of
                                      (True, True) -> 0
                                      (True, False) -> 1
                                      (False, False) -> 2
                                      (False, True) -> 3
                         angle = if x == 0
                                 then (maxBound, (signum y))
                                 else (quot y g, quot x g)
                         distance = x * x + y * y
                     in (quadrant, angle, distance, x, y))
  |> foldl (\m o@(q, a, d, x ,y) -> Map.insertWith (++) (q, a) [(d, x, y)] m)
           Map.empty
  |> Map.toList
  |> List.sortOn (\((q, (a, b)), _) -> (q, (fromIntegral a / fromIntegral b)))
  |> map (\(k, ls) -> List.sort ls)
  |> List.transpose
  |> concat
  |> map (\(d, x, y) -> (x, y))


solution :: [(Int, Int)] -> (Int, Int)
solution asteroids =
  let (sees, m, o) = asteroids
                   |> map (\(x0, y0) -> asteroids
                                     |> filter (/= (x0, y0))
                                     |> map (\(x, y) -> (x - x0, y - y0))
                                     |> (\ls -> (count_angles ls, ls, (x0, y0))))
                   |> List.sort
                   |> last
      kill_200 = m
                 |> kill_order
                 |> map (\(x, y) -> let (x0, y0) = o in 100 * (x + x0) + (y + y0))
                 |> (!! 199)
  in (sees, kill_200)
