{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields, FlexibleContexts, LambdaCase #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import qualified Data.List as List
import Data.List.Split
import Data.Set
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Control.Monad.State.Lazy as State

-- Complete the abbreviation function below.
abbreviation :: String -> String -> Bool
abbreviation x y =
  State.evalState (hw x y) Set.empty
  where (|>) data_structure function = function data_structure
        to_counts :: String -> Map.Map Char Int
        to_counts s = s |> List.map (\x -> (Char.toUpper x, 1))
                        |> Map.fromListWith (+)

        hw :: String -> String -> State.State (Set.Set (Int, Int)) Bool
        hw x y = let cx = to_counts x
                     cy = to_counts y
                 in
                   if Map.isSubmapOfBy (<=) cy cx
                   then h x y cx cy (length x) (length y) (length $ List.filter Char.isUpper x)
                   else return False

        has_enough :: Char -> Map.Map Char Int -> Map.Map Char Int -> Bool
        has_enough a cx cy = let nx = Map.findWithDefault 0 a cx
                                 ny = Map.findWithDefault 0 a cy
                             in
                               nx >= ny

        remove :: Char -> Map.Map Char Int -> Map.Map Char Int
        remove a cx = Map.adjust (\x -> x - 1) a cx

        h :: String -> String -> Map.Map Char Int -> Map.Map Char Int -> Int -> Int -> Int -> State.State (Set.Set (Int, Int)) Bool
        h x y cx cy dx dy ux = do
          s <- State.get
          if Set.member (dx, dy) s
          then return False
          else do
            State.modify (Set.insert (dx, dy))
            if dy > dx || ux > dy
            then return False
            else if dy == dx && x == y
            then return True
            else if ux == dy
            then return $ y == List.filter Char.isUpper x
            else case (x, y, dx, dy) of
                (_, _, 0, 0) -> return True
                (_, _, 0, _) -> return False
                (_, _, _, 0) -> return $ all Char.isLower x
                (a:as, b:bs, dx, dy) | a == b -> h as bs (remove a cx) (remove a cy) (dx - 1) (dy - 1) (ux - 1)
                                     | Char.isUpper a -> return False
                                     | Char.toUpper a /= b -> let new_cx = remove a cx
                                                              in
                                                                if has_enough a new_cx cy
                                                                then h as (b:bs) new_cx cy (dx - 1) dy ux
                                                                else return False
                                     | otherwise -> let new_cx = remove b cx
                                                        new_cy = remove b cy
                                                    in
                                                      h as bs new_cx new_cy (dx - 1) (dy - 1) ux >>= \case
                                                        True -> return True
                                                        False -> if has_enough a new_cx cy
                                                                 then h as (b:bs) new_cx cy (dx - 1) dy ux
                                                                 else return False

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
