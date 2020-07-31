module Day2
(
  parse
, solution
) where

import Data.Array (Array, (!), (//), listArray)
import qualified Data.List.Split as Split

parse :: String -> [Int]
parse s = map read $ Split.splitOn "," s

data Program = Program Int (Array Int Int)

extract :: Program -> (Int, Int, Int, Int)
extract (Program ic mem) =
  (mem ! ic, mem ! (ic + 1), mem ! (ic + 2), mem ! (ic + 3))

step :: Program -> Program
step p@(Program ic mem) =
  case extract p of
    (1, in1, in2, out) -> Program (ic + 4) (mem // [(out, calc (+) in1 in2)])
    (2, in1, in2, out) -> Program (ic + 4) (mem // [(out, calc (*) in1 in2)])
    (99, _, _, _) -> p
    _ -> undefined
  where calc f i1 i2 = f (mem ! i1) (mem ! i2)

finished :: Program -> Bool
finished (Program ic mem) = (mem ! ic) == 99

run :: Program -> Program
run p = if finished p then p else run $ step p

solution :: [Int] -> (Int, Int)
solution input =
  let init_mem = listArray (0, length input) input
      p_1202 = Program 0 (init_mem // [(1, 12), (2, 2)])
      (Program _ mem) = run p_1202
      (noun, verb) = head $ [(n, v) | n <- [0..99],
                                      v <- [0..99],
                                      (Program _ m) <- [run $ Program 0 (init_mem // [(1, n), (2, v)])],
                                      m ! 0 == 19690720]
  in (mem ! 0, 100 * noun + verb)
