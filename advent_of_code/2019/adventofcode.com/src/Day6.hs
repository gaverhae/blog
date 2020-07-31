{-# LANGUAGE LambdaCase #-}

module Day6
(
  parse
, solution
) where

import Lib ((|>))
import qualified Data.List.Split as Split
import qualified Data.Map.Strict as Map

data Tree = Tree String [Tree]

parse :: String -> [(String, String)]
parse s = s |> lines |> map (Split.splitOn ")") |> map (\case { [a, b] -> (a, b) })

parse_to_map :: [(String, String)] -> Map.Map String [String]
parse_to_map s = Map.fromListWith (++) $ map (\(c,o) -> (c, [o])) s

map_to_tree :: Map.Map String [String] -> Tree
map_to_tree m = build "COM"
  where build s = case Map.lookup s m of
          Nothing -> Tree s []
          Just orbiting_bodies -> Tree s (map build orbiting_bodies)

sum_of_depths :: Tree -> Int
sum_of_depths t = h 0 t
  where h d (Tree _ children) = d + (sum $ map (h (d + 1)) children)

solution :: [(String, String)] -> (Int, Int)
solution s =
  let orbits = parse_to_map s
      tree = map_to_tree orbits
  in (sum_of_depths tree, 0)
