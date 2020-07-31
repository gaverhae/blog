module Day5
(
  parse
, solution
) where

import qualified Lib as Lib

parse :: String -> [Int]
parse = Lib.comma_separated_ints

solution :: [Int] -> (Int, Int)
solution is = 
  let out1 = Lib.execIntcode [1] is
      out2 = Lib.execIntcode [5] is
  in (last out1, last out2)
