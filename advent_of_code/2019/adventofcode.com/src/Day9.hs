module Day9
(
  parse
, solution
) where

import qualified Lib

parse :: String -> [Int]
parse = Lib.comma_separated_ints

solution :: [Int] -> [Int]
solution is =
  let (Lib.Outputs keycode) = Lib.execIntcode (Lib.Inputs [1]) (Lib.Code is)
  in keycode
