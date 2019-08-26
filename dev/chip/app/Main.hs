module Main where

import Lib
import Data.Word (Word8)
import qualified Data.List as List
import qualified Data.Vector as Boxed
import qualified Data.Vector.Unboxed as Vector
import qualified Text.Printf as Printf

data ChipState = ChipState {
  memory :: Vector.Vector Word8,
  registers :: Vector.Vector Word8,
  address :: (Word8, Word8),
  program_counter :: Int,
  screen :: Boxed.Vector (Vector.Vector Bool)
}

blank_screen :: Boxed.Vector (Vector.Vector Bool)
blank_screen = Boxed.replicate 32 (Vector.replicate 64 False)

init :: [Word8] -> ChipState
init code = ChipState {
  memory = Vector.generate 4096 (\x -> if ((x - 0x200) >= 0 && (x - 0x200) < length code) then code !! (x - 0x200) else 0),
  registers = Vector.replicate 16 0,
  address = (0, 0),
  program_counter = 0x200,
  screen = blank_screen
}

decode :: Word8 -> (Int, Int)
decode i =
  let (a, b) = quotRem i 16
  in (fromIntegral a, fromIntegral b)

next_instruction :: ChipState -> (Int, Int, Int, Int)
next_instruction cs =
  let pc = program_counter cs
      (x, y) = decode $ (memory cs) Vector.! pc
      (z, t) = decode $ (memory cs) Vector.! (pc + 1)
  in (x, y, z, t)

clear_screen :: ChipState -> ChipState
clear_screen cs = cs { screen = blank_screen }

step :: ChipState -> ChipState
step cs = case next_instruction cs of
  (0, 0, 0xE, 0) -> clear_screen cs
  (0, _, _, _) -> error "jump to native not implemented"
  (a, b, c, d) -> error $ "unknown bytecode: " <> Printf.printf "0x%x%x%x%x" a b c d

print_screen :: ChipState -> String
print_screen cs =
  List.intercalate "\n" lines
  where s = screen cs
        lines = [line (s Boxed.! (i - 1) ) | i <- [1..(Boxed.length s)]]
        line v = [if (v Vector.! (i - 1)) then '#' else ' ' | i <- [1..(Vector.length v)]]

print_memory cs =
  List.intercalate "\n" lines
  where m = memory cs
        part n l = case l of {[] -> []; _ -> (take n l) : part n (drop n l)}
        mem_list = [m Vector.! (i - 1) | i <- [1..(Vector.length m)]]
        lines = [line r | r <- part 64 mem_list]
        line r = concat [Printf.printf "%02x%02x " x y | (x:y:[]) <- part 2 r]

maze :: [Word8]
maze = [0x60, 0x00, 0x61, 0x00, 0xa2, 0x22, 0xc2, 0x01, 0x32, 0x01, 0xa2, 0x1e, 0xd0, 0x14, 0x70, 0x04,
        0x30, 0x40, 0x12, 0x04, 0x60, 0x00, 0x71, 0x04, 0x31, 0x20, 0x12, 0x04, 0x12, 0x1c, 0x80, 0x40,
        0x20, 0x10, 0x20, 0x40, 0x80, 0x10]

main :: IO ()
main = do
  putStrLn $ print_memory $ Main.init []
  _ <- getLine
  putStrLn $ print_memory $ Main.init maze
  _ <- getLine
  putStrLn $ print_screen $ Main.init maze
  _ <- getLine
  putStrLn $ show $ next_instruction $ Main.init []
  putStrLn $ show $ next_instruction $ Main.init maze
--  putStrLn $ print_memory $ step $ Main.init maze
--  _ <- getLine
--  putStrLn $ print_screen $ step $ Main.init maze
