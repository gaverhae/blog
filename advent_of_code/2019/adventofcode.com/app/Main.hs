module Main where

import Lib
import qualified Day1 as Day1
import qualified Day2 as Day2
import qualified Day3 as Day3
import qualified Day4 as Day4

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
