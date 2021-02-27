module Main where

import Lib
import Data.Bits ((.&.))
import qualified Data.Bits as Bits
import Data.Word (Word8, Word16)
import Text.Printf (printf)
import qualified Data.List as List
import qualified Data.Vector as Boxed
import qualified Data.Vector.Unboxed as Vector
import Data.Vector.Unboxed (Vector)
import qualified Control.Monad.State.Lazy as State

data ChipState = ChipState {
  memory :: Vector Word8,
  registers :: Vector Word8,
  address :: Int,
  program_counter :: Int,
  screen :: Boxed.Vector (Vector Bool)
}

blank_screen :: Boxed.Vector (Vector Bool)
blank_screen = Boxed.replicate 32 (Vector.replicate 64 False)

-- Using Word16 for data entry for ntational convenience
init :: [Word16] -> ChipState
init code16 =
  let code = code16 >>= (\w16 -> let (a, b) = quotRem w16 256
                                 in [fromIntegral a, fromIntegral b])
  in ChipState {
  memory = Vector.generate 4096 (\x ->
    if (x - 0x200) >= 0
    && (x - 0x200) < length code
    then code !! (x - 0x200)
    else 0),
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
      (x, y) = decode $ memory cs Vector.! pc
      (z, t) = decode $ memory cs Vector.! (pc + 1)
  in (x, y, z, t)

clear_screen :: ChipState -> ChipState
clear_screen cs = cs { screen = blank_screen }

set_register :: ChipState -> Int -> Word8 -> ChipState
set_register cs register value =
  cs { registers = registers cs Vector.// [(register, value)] }

get_register :: ChipState -> Int -> Word8
get_register cs r = registers cs Vector.! r

set_address :: ChipState -> Int -> ChipState
set_address cs addr = cs { address = addr }

inc_pc :: ChipState -> ChipState
inc_pc cs = cs { program_counter = program_counter cs + 2 }

byte :: Int -> Int -> Word8
byte a b =
  if a < 0 || a >= 16 || b < 0 || b >= 16
  then error "should not happen"
  else fromIntegral $ a * 16 + b

trunc_word :: Int -> Word8
trunc_word i = fromIntegral $ abs i `rem` 256

-- parameters taken from
-- Saucier, R. (2000). Computer Generation of Statistical Distributions (1st
-- ed.). Aberdeen, MD. Army Research Lab.
-- via https://aaronschlegel.me/linear-congruential-generator-r.html
next_rand :: Int -> Int
next_rand prev = (1103515245 * prev + 12345) `rem` (2 ^ 32)

step :: ChipState -> State.State Int ChipState
step cs = case next_instruction cs of
  (0x0, 0x0, 0xE, 0x0) -> return $ inc_pc $ clear_screen cs
  (0x0,   _,   _,   _) -> error "jump to native not implemented"
  (0x3,   r,  n1,  n2) -> return $ inc_pc $ if get_register cs r == byte n1 n2 then inc_pc cs else cs
  (0x6,   r,  n1,  n2) -> return $ inc_pc $ set_register cs r $ byte n1 n2
  (0xa,  n1,  n2,  n3) -> return $ inc_pc $ set_address cs $ fromIntegral $ n1 * 256 + n2 * 16 + n3
  (0xc,   r,  n1,  n2) -> do
    State.modify next_rand
    rnd <- State.get
    return $ inc_pc $ set_register cs r $ trunc_word rnd .&. get_register cs r
  (0xd,  r1,  r2,   n) -> do
    let x_start = get_register cs r1
        y_start = get_register cs r2
        height = n
        current_screen = screen cs
        byte_to_bits b = [Bits.testBit b i | i <- [0..7]]
        bits = concat [byte_to_bits b | b <- [address cs..address cs + n - 1]]
        positions a xs = h a xs 0
          where h a [] n = []
                h a (x:xs) n | a == x = n : h a xs (n + 1)
                             | otherwise = h a xs (n + 1)
        indices_to_flip = map (\p -> (fromIntegral $ (p `rem` 8) + x_start,
                                      fromIntegral $ (p `quot` 8) + y_start))
                              $ positions True bits
        flip :: Boxed.Vector (Vector Bool) -> (Int, Int) -> Boxed.Vector (Vector Bool)
        flip s (i, j) = new_screen
          where row = s Boxed.! j
                new_screen = s Boxed.// [(j, row Vector.// [(i, not $ row Vector.! i)])]
        collision = False
    return cs{ screen = foldl flip (screen cs) indices_to_flip
             , registers = registers cs Vector.// [(15, if collision then 1 else 0)]}
  (a, b, c, d) -> error $ "unknown bytecode: " <> printf "0x%x%x%x%x" a b c d

step_n :: ChipState -> Int -> ChipState
step_n cs 0 = cs
step_n cs n =
  State.evalState (h cs n) 0
  where h :: ChipState -> Int -> State.State Int ChipState
        h cs 0 = return cs
        h cs n = do
          next_cs <- step cs
          h next_cs (n - 1)

print_screen :: ChipState -> String
print_screen cs =
  List.intercalate "\n" lines
  where s = screen cs
        lines = [line (s Boxed.! (i - 1)) | i <- [1..Boxed.length s]]
        line v = [if v Vector.! (i - 1) then '#' else ' '
                 | i <- [1..Vector.length v]]

print_memory :: ChipState -> String
print_memory cs =
  List.intercalate "\n" lines
  where lines = x_offsets : [line r idx
                            | (r, idx) <- zip (part 16 mem_list) [0..]
                            , not (all (== 0) r)]
        x_offsets = "     " ++ concat [printf ".%x.%x " (x * 2) ((x * 2) + 1)
                                      | x <- [0..7] :: [Word8]]
        line r idx = concat $ printf "%03x. " (idx::Int) : [printf "%02x%02x " x y | [x, y] <- part 2 r]
        part n l = case l of {[] -> []; _ -> take n l : part n (drop n l)}
        mem_list = [m Vector.! (i - 1) | i <- [1..Vector.length m]]
        m = memory cs

maze :: [Word16]
maze = [0x6000, 0x6100, 0xa222, 0xc201, 0x3201, 0xa21e, 0xd014, 0x7004,
        0x3040, 0x1204, 0x6000, 0x7104, 0x3120, 0x1204, 0x121c, 0x8040,
        0x2010, 0x2040, 0x8010]

-- https://archive.org/details/byte-magazine-1978-12/page/n113/mode/2up
print_ship :: [Word16]
print_ship = [0x6200, 0x6300, 0xa20a, 0xd236, 0x1208, 0x2070, 0x70f8, 0xd888]

print_state :: ChipState -> IO ()
print_state cs = do
  putStrLn $ print_memory cs
  putStrLn $ printf "PC: %8x, Rs: %sI: %8x" (program_counter cs) ((concatMap (printf "%02x, ") $ Vector.toList (registers cs)) :: String) (address cs)
  putStrLn $ print_screen cs

main :: IO ()
main = do
  let state = step_n (Main.init print_ship) 100
  print_state state
