module Main where

import Lib
import qualified Day1 as Day1

main :: IO ()
main = do
  day1_input <- readFile "inputs/day1"
  putStrLn $ "Solution for Day1: " <> show (Day1.solution $ map read $ lines day1_input)
