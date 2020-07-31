module Day5
(
  parse
, solution
) where

import qualified Lib as Lib

parse :: String -> [Int]
parse = Lib.comma_separated_ints

solution :: [Int] -> (Int, Int)
solution is = case (Lib.execIntcode [1] is, Lib.execIntcode [5] is) of
  (Lib.Success o1, Lib.Success o2) -> (o1, o2)
  (Lib.Fail err, _) -> error $ show err
  (_, Lib.Fail err) -> error $ show err
