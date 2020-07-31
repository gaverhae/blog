module Day1
(
  parse
, solution
) where

parse :: String -> [Int]
parse s = map read $ lines s

fuel_for_weight :: Int -> Int
fuel_for_weight w = (w `quot` 3) - 2

fuel_for_weight_and_fuel :: Int -> Int
fuel_for_weight_and_fuel w =
  sum $ drop 1 $ takeWhile (> 0) $ iterate fuel_for_weight w

solution :: [Int] -> (Int, Int)
solution module_weights =
  let fuel_for_modules = sum $ map fuel_for_weight module_weights
      fuel_for_modules_and_fuel = sum $ map fuel_for_weight_and_fuel module_weights
  in (fuel_for_modules, fuel_for_modules_and_fuel)
