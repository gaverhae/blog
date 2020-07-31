module Day4
(
  parse
, solution
) where

import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Set as Set

parse :: String -> (Int, Int)
parse s = case Split.splitOn "-" s of
  [s1, s2] -> (read s1, read s2)
  _ -> undefined

solution :: (Int, Int) -> (Int, Int)
solution (min, max) = (size, smaller_size)
  where part_1_list = [n | n <- [min .. max],
                           s <- [show n],
                           s == List.sort s,
                           not $ null $ filter pred $ Split.divvy 2 1 s]
        size = length part_1_list
        pred ab = case ab of
          [a, b] -> a == b
          _ -> undefined
        smaller_size = length $ filter pred_part_2 part_1_list
        pred_part_2 n =
          let subs = Set.fromList $ List.subsequences $ show n
          in not $ null $ [c | c <- show n,
                               [c, c] `Set.member` subs,
                               not $ [c, c, c] `Set.member` subs]
