module Chapter9 where

import System.IO

getCh = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x
