{-# LANGUAGE LambdaCase #-}

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

checksum :: [[Int]] -> Int
checksum layers =
  layers
  |> List.sortOn (countEl 0)
  |> head
  |> (\l -> (countEl 1 l) * (countEl 2 l))
  where countEl e ls = length $ filter (== e) ls
 
decode :: [[Int]] -> String
decode layers =
  foldl merge (head layers) (tail layers)
  |> map show |> concat
  where merge acc el = map combine $ zip acc el
        combine = (\case {(2, n) -> n; (o, _) -> o})

solution :: [Int] -> String
solution is =
  let layers = Split.chunksOf (25 * 6) is
      cs = checksum layers
      msg = decode layers
          |> map (\case {'1' -> '#'; '0' -> ' '})
          |> Split.chunksOf 25
  in unlines $ show cs:msg
