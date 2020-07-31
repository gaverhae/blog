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
  let (Lib.Outputs out1) = Lib.execIntcode (Lib.Inputs [1]) (Lib.Code is)
      (Lib.Outputs out2) = Lib.execIntcode (Lib.Inputs [5]) (Lib.Code is)
  in (last out1, last out2)
