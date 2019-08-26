module Main where

import Lib
import Data.Word (Word8)
import qualified Data.List as List
import qualified Data.Vector as Boxed
import qualified Data.Vector.Unboxed as Vector
import Text.Printf (printf)

data ChipState = ChipState {
  memory :: Vector.Vector Word8,
  registers :: Vector.Vector Word8,
  address :: Int,
  program_counter :: Int,
  screen :: Boxed.Vector (Vector.Vector Bool)
}

blank_screen :: Boxed.Vector (Vector.Vector Bool)
blank_screen = Boxed.replicate 32 (Vector.replicate 64 False)

init :: [Word8] -> ChipState
init code = ChipState {
  memory = Vector.generate 4096 (\x -> if ((x - 0x200) >= 0 && (x - 0x200) < length code) then code !! (x - 0x200) else 0),
  registers = Vector.replicate 16 0,
  address = 0,
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

set_register :: ChipState -> Int -> Word8 -> ChipState
set_register cs register value =
  cs { registers = ((registers cs) Vector.// [(register, value)]) }

inc_pc :: ChipState -> ChipState
inc_pc cs = cs { program_counter = program_counter cs + 2 }

step :: ChipState -> ChipState
step cs = case next_instruction cs of
  (0x0, 0x0, 0xE, 0x0) -> inc_pc $ clear_screen cs
  (0x0,   _,   _,   _) -> error "jump to native not implemented"
  (0x6,   r,   a,   b) -> inc_pc $ set_register cs r $ fromIntegral (a * 16 + b)
  (a, b, c, d) -> error $ "unknown bytecode: " <> printf "0x%x%x%x%x" a b c d

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
        line r = concat [printf "%02x%02x " x y | (x:y:[]) <- part 2 r]

maze :: [Word8]
maze = [0x60, 0x00, 0x61, 0x00, 0xa2, 0x22, 0xc2, 0x01, 0x32, 0x01, 0xa2, 0x1e, 0xd0, 0x14, 0x70, 0x04,
        0x30, 0x40, 0x12, 0x04, 0x60, 0x00, 0x71, 0x04, 0x31, 0x20, 0x12, 0x04, 0x12, 0x1c, 0x80, 0x40,
        0x20, 0x10, 0x20, 0x40, 0x80, 0x10]

step_n :: ChipState -> Int -> ChipState
step_n cs 0 = cs
step_n cs n = step_n (step cs) (n - 1)

print_state :: ChipState -> IO ()
print_state cs = do
  putStrLn $ print_memory $ cs
  putStrLn $ printf "PC: %8x, Rs: %sI: %8x" (program_counter cs) ((concatMap (\x -> printf "%02x, " x) $ Vector.toList (registers cs)) :: String) (address cs)
  putStrLn $ print_screen $ cs

main :: IO ()
main = do
  let state = step_n (Main.init maze) 2
  print_state state
