module Day10
(
  parse,
  solution
) where

import Lib ((|>))
import qualified Data.List as List
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

solution :: [(Int, Int)] -> Int
solution asteroids =
  let sees_most = asteroids
                |> map (\(x0, y0) -> asteroids
                                  |> filter (/= (x0, y0))
                                  |> map (\(x, y) -> (x - x0, y - y0))
                                  |> count_angles)
                |> List.sort
                |> last
  in sees_most
