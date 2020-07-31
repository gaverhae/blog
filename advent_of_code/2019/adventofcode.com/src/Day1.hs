module Day1
(
  solution
) where

fuel_for_module :: Int -> Int
fuel_for_module m = (m `quot` 3) - 2

solution :: [Int] -> Int
solution module_weights = sum $ map fuel_for_module module_weights
