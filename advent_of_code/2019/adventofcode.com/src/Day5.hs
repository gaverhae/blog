module Day5
(
  parse
, solution
) where

import qualified Lib as Lib

parse :: String -> [Int]
parse = Lib.comma_separated_ints

solution :: [Int] -> (Int, Int)
solution is = case Lib.execIntcode [1] is of
  Lib.Success out -> (out, 0)
  Lib.Fail err -> error $ show err
