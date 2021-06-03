module Chapter9 where

import System.IO

getCh :: IO Char
getCh = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

readLine :: IO String
readLine = loop []
  where
  loop :: String -> IO String
  loop line = do
    c <- getCh
    case c of
      '\n' -> do
        putChar c
        return line
      '\DEL' -> do
        case line of
          [] -> loop []
          xs -> do
            putStr "\ESC[1D \ESC[1D"
            loop (init line)
      _ -> do
        putChar c
        loop (line ++ [c])
