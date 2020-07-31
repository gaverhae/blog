module Day4
(
  parse
, solution
) where

import qualified Data.List as List
import qualified Data.List.Split as Split

parse :: String -> (Int, Int)
parse s = case Split.splitOn "-" s of
  [s1, s2] -> (read s1, read s2)
  _ -> undefined

solution :: (Int, Int) -> (Int, Int)
solution (min, max) = (size, 0)
  where size = length [n | n <- [min .. max],
                           s <- [show n],
                           s == List.sort s,
                           not $ null $ filter pred $ Split.divvy 2 1 s]
        pred ab = case ab of
          [a, b] -> a == b
          _ -> undefined
