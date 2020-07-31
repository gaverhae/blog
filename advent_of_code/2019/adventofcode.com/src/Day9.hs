module Day9
(
  parse
, solution
) where

import qualified Lib

parse :: String -> [Int]
parse = Lib.comma_separated_ints

solution :: [Int] -> (Int, Int)
solution is =
  let (Lib.Outputs [keycode]) = Lib.execIntcode (Lib.Inputs [1]) (Lib.Code is)
      (Lib.Outputs [coords]) = Lib.execIntcode (Lib.Inputs [2]) (Lib.Code is)
  in (keycode, coords)
