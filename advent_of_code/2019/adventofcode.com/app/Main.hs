module Main where

import Lib ((|>))
import Data.Ratio ((%))
import qualified Data.Ratio
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

asteroids :: [(Int, Int)]
asteroids =
  [(-7,-3),
   (-2,-3),
   (-1,-3),
   (0,-3),
   (1,-3),
   (2,-3),
   (6,-3),
   (-8,-2),
   (-7,-2),
   (0,-2)]

asteroids_angle :: [(Data.Ratio.Ratio Int, Int, Int)]
asteroids_angle =
  [(3 % 7,-7,-3),
   (3 % 2,-2,-3),
   (3 % 1,-1,-3),
   ((-9223372036854775807) % 1,0,-3),
   ((-3) % 1,1,-3),
   ((-3) % 2,2,-3),
   ((-1) % 2,6,-3),
   (1 % 4,-8,-2),
   (2 % 7,-7,-2),
   ((-9223372036854775807) % 1,0,-2)]


buggy :: String
buggy =
  asteroids
  |> map (\(x, y) -> let angle = if x == 0
                                 then (maxBound % (signum y))
                                 else (y % x)
                     in (angle, x, y))
  |> foldl (\m (a, x ,y) -> Map.insertWith (++) (a) [(x, y)] m)
           Map.empty
  |> Map.toList
  |> List.sort
  |> map show
  |> unlines

correct :: String
correct =
  asteroids_angle
  |> foldl (\m (a, x ,y) -> Map.insertWith (++) (a) [(x, y)] m)
           Map.empty
  |> Map.toList
  |> List.sort
  |> map show
  |> unlines

main :: IO ()
main = do
  putStrLn buggy
  putStrLn "--"
  putStrLn correct
  putStrLn "--"
  putStrLn "-"

{-
-- prints:
((-9223372036854775807) % 1,[(0,-3)])
((-3) % 1,[(1,-3)])
((-3) % 2,[(2,-3)])
((-1) % 2,[(6,-3)])
(1 % 4,[(-8,-2)])
((-9223372036854775807) % 1,[(0,-2)])
(2 % 7,[(-7,-2)])
(3 % 7,[(-7,-3)])
(3 % 2,[(-2,-3)])
(3 % 1,[(-1,-3)])
-}
