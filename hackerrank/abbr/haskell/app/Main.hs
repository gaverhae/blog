{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields, FlexibleContexts #-}

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
import qualified Control.Monad.State.Lazy as State

-- Complete the abbreviation function below.
abbreviation :: String -> String -> Bool
abbreviation x y =
  State.evalState (h x y) $ Set.empty
  where h x y = do
          s <- State.get
          if Set.member (x, y) s
          then return False
          else do
            State.modify (Set.insert (x, y))
            case (x, y) of
              ([], []) -> return True
              ([], y) -> return False
              (x, []) -> return $ all Char.isLower x
              (a:as, b:bs) | a == b -> h as bs
                           | Char.isUpper a -> return False
                           | Char.toUpper a /= b -> h as (b:bs)
                           | otherwise -> do
                               withUpper <- h as bs
                               if withUpper then return True else h as (b:bs)

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
