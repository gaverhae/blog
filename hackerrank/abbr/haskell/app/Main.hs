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
import qualified Data.Set as Set

-- Complete the abbreviation function below.
abbreviation :: String -> String -> Bool
abbreviation x y =
  snd $ h mem x y
    where
      mem = Set.empty :: Set (String, String)
      h mem' x y =
        if Set.member (x,y) mem'
        then (mem', False)
        else let mem'' = Set.insert (x,y) mem'
             in case (x, y) of
               ([], []) -> (mem'', True)
               ([], y) -> (mem'', False)
               (x, []) -> (mem'', all Char.isLower x)
               (a:as, b:bs) | a == b -> h mem'' as bs
                            | Char.isUpper a -> (mem'', False)
                            | otherwise -> case h mem'' ((Char.toUpper a):as) (b:bs) of
                                             (mem''', True) -> (mem''', True)
                                             (mem''', False) -> h mem''' as (b:bs)

ab_wrap :: String -> String -> String
ab_wrap a b = if abbreviation a b then "YES" else "NO"

main :: IO()
main = do
  stdout <- getEnv "OUTPUT_PATH"
  fptr <- openFile stdout WriteMode
  q <- readLn :: IO Int
  forM_ [1..q] $ \q_itr -> do
    a <- getLine
    b <- getLine
    let result = ab_wrap a b
    hPutStrLn fptr result
  hFlush fptr
  hClose fptr
