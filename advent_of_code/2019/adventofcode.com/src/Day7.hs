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

chain :: [Int] -> [Int] -> Int
chain code phase_sequence =
  foldl run_machine 0 phase_sequence
  where run_machine previous_output phase_setting =
          case Lib.execIntcode [phase_setting, previous_output] code of
            [n] -> n
            _ -> undefined

feedback_loop code [pa, pb, pc, pd, pe] =
  let run inputs = Lib.execIntcode inputs code
      a_out = run (pa:0:e_out)
      b_out = run (pb:a_out)
      c_out = run (pc:b_out)
      d_out = run (pd:c_out)
      e_out = run (pe:d_out)
  in last e_out
feedback_loop _ _ = undefined

solution :: [Int] -> (Int, Int)
solution is =
  let max_out = sequences 4
              |> map (chain is)
              |> List.sort
              |> last
      max_out_feedback = sequences 4
                       |> map (map (+ 5))
                       |> map (feedback_loop is)
                       |> List.sort
                       |> last
  in (max_out, max_out_feedback)
