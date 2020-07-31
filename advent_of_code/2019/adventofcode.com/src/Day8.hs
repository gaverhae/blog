module Day8
(
  parse
, solution
) where

import qualified Data.List as List
import qualified Data.List.Split as Split
import Lib ((|>))

parse :: String -> [Int]
parse s = s |> init |> map (:[]) |> map read

solution :: [Int] -> (Int, Int)
solution is =
  let checksum = is
               |> Split.chunksOf (25 * 6)
               |> List.sortOn (countEl 0)
               |> head
               |> (\l -> (countEl 1 l) * (countEl 2 l))
      countEl e ls = length $ filter (== e) ls
  in (checksum, 0)
