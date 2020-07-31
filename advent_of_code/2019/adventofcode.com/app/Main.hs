module Main where

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10

ensure :: (Eq a, Show a) => a -> a -> IO ()
ensure x y = do
  if x == y
  then return ()
  else error $ "expected " ++ show x ++ " but got " ++ show y

main :: IO ()
main = do
  day1_input <- readFile "inputs/day1"
  ensure (3394032, 5088176) (Day1.solution $ Day1.parse day1_input)
  day2_input <- readFile "inputs/day2"
  ensure (3058646, 8976) (Day2.solution $ Day2.parse day2_input)
  day3_input <- readFile "inputs/day3"
  ensure (217, 3454) (Day3.solution $ Day3.parse day3_input)
  day4_input <- readFile "inputs/day4"
  ensure (1019, 660) (Day4.solution $ Day4.parse day4_input)
  day5_input <- readFile "inputs/day5"
  ensure (16348437, 6959377) (Day5.solution $ Day5.parse day5_input)
  day6_input <- readFile "inputs/day6"
  ensure (621125, 550) (Day6.solution $ Day6.parse day6_input)
  day7_input <- readFile "inputs/day7"
  ensure (92663, 14365052) (Day7.solution $ Day7.parse day7_input)
  day8_input <- readFile "inputs/day8"
  ensure (unlines ["1806",
                   "  ##  ##  #### ###   ##  ",
                   "   # #  # #    #  # #  # ",
                   "   # #  # ###  #  # #  # ",
                   "   # #### #    ###  #### ",
                   "#  # #  # #    # #  #  # ",
                   " ##  #  # #    #  # #  # "])
         (Day8.solution $ Day8.parse day8_input)
  day9_input <- readFile "inputs/day9"
  ensure (3460311188, 42202) (Day9.solution $ Day9.parse day9_input)
  day10_input <- readFile "inputs/day10"
  ensure (314, 1513) (Day10.solution $ Day10.parse day10_input)
