{-# LANGUAGE LambdaCase #-}

module Day6
(
  parse
, solution
) where

import Lib ((|>))
import qualified Data.List.Split as Split
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data Tree = Tree String [Tree]

parse :: String -> [(String, String)]
parse s = s |> lines |> map (Split.splitOn ")") |> map (\case { [a, b] -> (a, b) })

parse_to_tree :: [(String, String)] -> Tree
parse_to_tree s = build "COM"
  where build s = case Map.lookup s m of
          Nothing -> Tree s []
          Just orbiting_bodies -> Tree s (map build orbiting_bodies)
        m = Map.fromListWith (++) $ map (\(c,o) -> (c, [o])) s

sum_of_depths :: Tree -> Int
sum_of_depths t = h 0 t
  where h d (Tree _ children) = d + (sum $ map (h (d + 1)) children)

parse_to_graph :: [(String, String)] -> Map.Map String (Set.Set String)
parse_to_graph s = Map.fromListWith Set.union two_way_links
  where two_way_links = concatMap (\(c,o) -> [(c, (Set.singleton o)), (o, (Set.singleton c))]) s

shortest_path :: Map.Map String (Set.Set String) -> String -> String -> Int
shortest_path graph start end = explore 0 Set.empty (Set.fromList [start])
  where explore d seen cur =
          if Set.member end cur
          then d
          else let new_seen = (cur `Set.union` seen)
                   next_visit = (Set.unions $ Set.map get_neighbours cur) `Set.difference` new_seen
               in explore (d + 1) new_seen next_visit
        get_neighbours s = case Map.lookup s graph of
          Nothing -> Set.empty
          Just s -> s

solution :: [(String, String)] -> (Int, Int)
solution s =
  let tree = parse_to_tree s
      graph = parse_to_graph s
  in (sum_of_depths tree, shortest_path graph "YOU" "SAN" - 2)
