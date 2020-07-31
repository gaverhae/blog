module Main where

import qualified Day1 as Day1
import qualified Day2 as Day2
import qualified Day3 as Day3
import qualified Day4 as Day4
import qualified Day5 as Day5
import qualified Day6 as Day6
import qualified Day7 as Day7
import qualified Day8 as Day8

main :: IO ()
main = do
  day1_input <- readFile "inputs/day1"
  putStrLn $ "Day1: " <> show (Day1.solution $ Day1.parse day1_input)
  day2_input <- readFile "inputs/day2"
  putStrLn $ "Day2: " <> show (Day2.solution $ Day2.parse day2_input)
  day3_input <- readFile "inputs/day3"
  putStrLn $ "Day3: " <> show (Day3.solution $ Day3.parse day3_input)
  day4_input <- readFile "inputs/day4"
  putStrLn $ "Day4: " <> show (Day4.solution $ Day4.parse day4_input)
  day5_input <- readFile "inputs/day5"
  putStrLn $ "Day5: " <> show (Day5.solution $ Day5.parse day5_input)
  day6_input <- readFile "inputs/day6"
  putStrLn $ "Day6: " <> show (Day6.solution $ Day6.parse day6_input)
  day7_input <- readFile "inputs/day7"
  putStrLn $ "Day7: " <> show (Day7.solution $ Day7.parse day7_input)
  day8_input <- readFile "inputs/day8"
  putStrLn $ "Day8: " <> show (Day8.solution $ Day8.parse day8_input)
