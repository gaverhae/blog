module Day7
(
  parse
, solution
) where

import qualified Data.List as List
import qualified Lib as Lib
import Lib ((|>))

parse :: String -> [Int]
parse = Lib.comma_separated_ints

sequences :: Int -> [[Int]]
sequences n = helper [0..n]
  where helper [] = []
        helper [a] = [[a]]
        helper ls = concatMap (\e -> map (e:) (helper (filter (/= e) ls))) ls

chain :: [Int] -> Int -> [Int] -> Int
chain code initial_input phase_sequence =
  foldl run_machine initial_input phase_sequence
  where run_machine previous_output phase_setting =
          case Lib.execIntcode [phase_setting, previous_output] code of
            Lib.Success n -> n
            Lib.Fail ls -> error $ show ls

solution :: [Int] -> (Int, Int)
solution is =
  let max_out = sequences 4
              |> map (chain is 0)
              |> List.sort
              |> last
  in (max_out, 0)
