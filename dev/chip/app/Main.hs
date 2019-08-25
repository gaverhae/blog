module Main where

import Lib
import Data.Int (Int8)
import qualified Data.Vector.Unboxed as Vector

data ChipState = ChipState { memory :: Vector.Vector Int8, registers :: Vector.Vector Int8 }

init :: ChipState
init = ChipState { memory = Vector.replicate 4000 0, registers = Vector.replicate 16 0 }


main :: IO ()
main = someFunc
