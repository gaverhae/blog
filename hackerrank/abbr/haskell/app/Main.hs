{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe
import qualified Data.Char as Char

-- Complete the abbreviation function below.
abbreviation :: String -> String -> String
abbreviation x y = case ab x y of
  True -> "YES"
  False -> "NO"
  where ab [] [] = True
        ab [] bs = False
        ab (a:as) [] | Char.isUpper a = False
                     | otherwise = ab as []
        ab (a:as) (b:bs) | a == b = ab as bs
                         | Char.isUpper a = False
                         | otherwise = ab ((Char.toUpper a):as) (b:bs)
                                       || ab as (b:bs)

main :: IO()
main = do
  stdout <- getEnv "OUTPUT_PATH"
  fptr <- openFile stdout WriteMode
  q <- readLn :: IO Int
  forM_ [1..q] $ \q_itr -> do
    a <- getLine
    b <- getLine
    let result = abbreviation a b
    hPutStrLn fptr result
  hFlush fptr
  hClose fptr
