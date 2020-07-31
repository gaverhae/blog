module Day3
(
  parse
, solution
) where

import Prelude hiding (Left,Right)
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Set as Set

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

path :: [Move] -> Set.Set (Int, Int)
path ms = walk Set.empty ms (0, 0)
  where walk been [] _ = been
        walk been ((Move direction distance):t) pos = walk new_been t new_pos
          where path = take distance $ drop 1 $ iterate (move direction) pos
                new_pos = last path
                new_been = been `Set.union` (Set.fromList path)
                move Right (x, y) = (x + 1, y    )
                move Left  (x, y) = (x - 1, y    )
                move Up    (x, y) = (x    , y + 1)
                move Down  (x, y) = (x    , y - 1)

distance_to_o :: (Int, Int) -> Int
distance_to_o (x, y) = (abs x) + (abs y)

(|>) :: a -> (a -> b) -> b
(|>) a f = f a

solution :: ([Move], [Move]) -> (Int, Int)
solution (l1, l2) = (closest_cross, 0)
  where closest_cross = (path l1) `Set.intersection` (path l2)
                      |> Set.toList
                      |> map distance_to_o
                      |> List.sort
                      |> head
