module Day3
(
  parse
, solution
) where

import Prelude hiding (Left,Right)
import Lib ((|>))
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

data Direction = Left | Right | Up | Down
data Move = Move Direction Int

parse :: String -> ([Move], [Move])
parse s =
  case (map f $ lines s) of
    [l1, l2] -> (l1, l2)
    _ -> undefined
  where f l = map p $ Split.splitOn "," l
        p (d:h) = Move dir (read h)
          where dir = case d of
                  'R' -> Right
                  'L' -> Left
                  'U' -> Up
                  'D' -> Down
                  _ -> undefined

path :: [Move] -> Map.Map (Int, Int) Int
path ms = walk Map.empty ms ((0, 0), 0)
  where walk been [] _ = been
        walk been ((Move direction distance):t) pos = walk new_been t new_pos
          where path = take distance $ drop 1 $ iterate (move direction) pos
                new_pos = last path
                -- Map.union default to left value if conflict
                new_been = been `Map.union` (Map.fromList path)
                move Right ((x, y), d) = ((x + 1, y    ), d + 1)
                move Left  ((x, y), d) = ((x - 1, y    ), d + 1)
                move Up    ((x, y), d) = ((x    , y + 1), d + 1)
                move Down  ((x, y), d) = ((x    , y - 1), d + 1)

distance_to_o :: (Int, Int) -> Int
distance_to_o (x, y) = (abs x) + (abs y)

solution :: ([Move], [Move]) -> (Int, Int)
solution (l1, l2) = (closest_cross, fastest_cross)
  where closest_cross = (Map.keysSet $ path l1) `Set.intersection` (Map.keysSet $ path l2)
                      |> Set.toList
                      |> map distance_to_o
                      |> List.sort
                      |> head
        fastest_cross = Map.intersectionWith (+) (path l1) (path l2)
                      |> Map.elems
                      |> List.sort
                      |> head
