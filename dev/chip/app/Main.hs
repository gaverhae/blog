module Main where

import Data.Bits ((.&.))
import qualified Data.Bits as Bits
import Data.Word (Word8, Word16)
import qualified Debug.Trace
import Text.Printf (printf)
import qualified Data.List as List
import qualified Data.Vector as Boxed
import qualified Data.Vector.Unboxed as Vector
import Data.Vector.Unboxed (Vector)
import qualified Data.Set as Set
import qualified Control.Monad.State.Lazy as State
import Control.Monad.State.Lazy (State)

data ChipState = ChipState {
  memory :: Vector Word8,
  registers :: Vector Word8,
  address :: Int,
  program_counter :: Int,
  stack :: [Int],
  screen :: Boxed.Vector (Vector Bool)
} deriving (Eq)

blank_screen :: Boxed.Vector (Vector Bool)
blank_screen = Boxed.replicate 32 (Vector.replicate 64 False)

-- Using Word16 for data entry for ntational convenience
init :: [Word16] -> ChipState
init code16 =
  let code = code16 >>= (\w16 -> let (a, b) = quotRem w16 256
                                 in [fromIntegral a, fromIntegral b])
  in ChipState {
  memory = Vector.generate 4096 (\x ->
    case x of
      -- 0
      0 -> 0x70; 1 -> 0x50; 2 -> 0x50; 3 -> 0x50; 4 -> 0x70
      -- 1
      5 -> 0x20; 6 -> 0x60; 7 -> 0x20; 8 -> 0x20; 9 -> 0x20
      -- 2
      10 -> 0x70; 11 -> 0x10; 12 -> 0x70; 13 -> 0x40; 14 -> 0x70
      -- 3
      15 -> 0x70; 16 -> 0x10; 17 -> 0x70; 18 -> 0x10; 19 -> 0x70
      -- 4
      20 -> 0x50; 21 -> 0x50; 22 -> 0x70; 23 -> 0x10; 24 -> 0x10
      -- 5
      25 -> 0x70; 26 -> 0x40; 27 -> 0x70; 28 -> 0x10; 29 -> 0x70
      -- 6
      30 -> 0x70; 31 -> 0x40; 32 -> 0x70; 33 -> 0x50; 34 -> 0x70
      -- 7
      35 -> 0x70; 36 -> 0x10; 37 -> 0x10; 38 -> 0x10; 39 -> 0x10
      -- 8
      40 -> 0x70; 41 -> 0x50; 42 -> 0x70; 43 -> 0x50; 44 -> 0x70
      -- 9
      45 -> 0x70; 46 -> 0x50; 47 -> 0x70; 48 -> 0x10; 49 -> 0x70
      x | (x - 0x200) >= 0 && (x - 0x200) < length code -> code !! (x - 0x200)
      _ -> 0),
  registers = Vector.replicate 16 0,
  address = 0,
  program_counter = 0x200,
  stack = [],
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

bcd_register :: ChipState -> Int -> ChipState
bcd_register cs x =
  let r = get_register cs x
      (h, r') = r `quotRem` 100
      (d, u) = r' `quotRem` 10
      ad = address cs
      new_mem = memory cs Vector.// [(ad, h), (ad+1, d), (ad+2, u)]
  in cs { memory = new_mem }

get_memory_at :: ChipState -> Int -> Word8
get_memory_at cs offset = memory cs Vector.! offset

get_screen_at :: ChipState -> Word8 -> Word8 -> [Word8]
get_screen_at cs wx wy =
  let [x, y] = map fromIntegral [wx, wy]
      max_x = min 63 (x+7)
      max_y = min 31 (y+5)
  in [byte | dy <- [y..max_y]
           , let rbits = [(screen cs Boxed.! (dy)) Vector.! (dx) | dx <- [x..max_x]]
           , let byte = sum [2 ^ exp | (b, exp) <- zip rbits [(0::Int)..]
                                     , b]]

display :: Boxed.Vector (Vector Bool) -> Word8 -> Word8 -> [Word8] -> Boxed.Vector (Vector Bool)
display screen wx wy pattern =
  let [x, y] = map fromIntegral [wx, wy]
      pattern_indices = Set.fromList [ (y+dy, x+dx) | dy <- [0..length pattern - 1]
                                                    , let pattern_byte = pattern !! dy
                                                    , dx <- [0..7]
                                                    , Bits.testBit pattern_byte (7 - dx) ]
  in Boxed.imap (\y line ->
       Vector.imap (\x b ->
                     if (y, x) `Set.member` pattern_indices
                     then not b
                     else b)
                   line)
                screen

draw :: Int -> Int -> Int -> ChipState -> ChipState
draw r1 r2 n cs =
  let x = get_register cs r1
      y = get_register cs r2
      to_draw = [get_memory_at cs addr | addr <- [address cs..address cs + n - 1]]
      on_screen = take n $ get_screen_at cs x y
      collisions = [ td .&. os | (td, os) <- zip to_draw on_screen]
      collision_happened = not (all (== 0) collisions)
      new_screen = display (screen cs) x y to_draw
  in if (x < 0 || x >= 64)
     || (y < 0 || y >= 32)
     then error $ "invalid coordinates: (" <> show x <> ", " <> show y <> ")"
     else cs { screen = new_screen
             , registers = registers cs Vector.// [(0xf, sum [1 | collision_happened])]}

inc_pc :: ChipState -> ChipState
inc_pc cs = cs { program_counter = program_counter cs + 2 }

set_pc :: ChipState -> Int -> ChipState
set_pc cs n = cs { program_counter = n }

byte :: Int -> Int -> Word8
byte a b =
  if a < 0 || a >= 16 || b < 0 || b >= 16
  then error "should not happen"
  else fromIntegral $ a * 16 + b

byte3 :: Int -> Int -> Int -> Int
byte3 a b c = 256 * a + 16 * b + c

trunc_rand :: Int -> Word8
trunc_rand i = fromIntegral $ Bits.shift i (-8)

-- parameters taken from
-- Saucier, R. (2000). Computer Generation of Statistical Distributions (1st
-- ed.). Aberdeen, MD. Army Research Lab.
-- via https://aaronschlegel.me/linear-congruential-generator-r.html
next_rand :: Int -> Int
next_rand prev = (1103515245 * prev + 12345) `rem` ((2::Int) ^ (32::Int))

load_registers :: ChipState -> Int -> ChipState
load_registers cs max =
  let regs = [ (idx, fromIntegral val) | idx <- [0..max]
                                       , let val = get_memory_at cs (address cs + idx) ]
  in cs { address = address cs + max + 1,
          registers = registers cs Vector.// regs }

step :: ChipState -> State Int ChipState
step cs = case next_instruction cs of
  (0x0, 0x0, 0xe, 0x0) -> return $ inc_pc $ clear_screen cs
  (0x0,   _,   _,   _) -> error "jump to native not implemented"
  (0x1,  m1,  m2,  m3) -> return $ set_pc cs $ byte3 m1 m2 m3
  (0x2,  m1,  m2,  m3) -> return cs{ stack = program_counter cs: stack cs, program_counter = byte3 m1 m2 m3 }
  (0x3,   r,  n1,  n2) -> return $ inc_pc $ if get_register cs r == byte n1 n2 then inc_pc cs else cs
  (0x6,   r,  n1,  n2) -> return $ inc_pc $ set_register cs r $ byte n1 n2
  (0x7,   r,  n1,  n2) -> return $ inc_pc $ set_register cs r $ (get_register cs r + byte n1 n2)
  (0xa,  n1,  n2,  n3) -> return $ inc_pc $ set_address cs $ byte3 n1 n2 n3
  (0xc,   r,  n1,  n2) -> do
    State.modify next_rand
    rnd <- State.get
    return $ inc_pc $ set_register cs r $ trunc_rand rnd .&. byte n1 n2
  (0xd,  r1,  r2,   n) -> return $ inc_pc $ draw r1 r2 n cs
  (0xf,   x, 0x2, 0x9) -> return $ inc_pc $ set_address cs $ fromIntegral $ 5 * (get_register cs x `rem` 10)
  (0xf,   x, 0x3, 0x3) -> return $ inc_pc $ bcd_register cs x
  (0xf,   x, 0x6, 0x5) -> return $ inc_pc $ load_registers cs x
  (a, b, c, d) -> error $ "unknown bytecode: " <> printf "0x%x%x%x%x" a b c d

step_n :: ChipState -> Int -> ChipState
step_n cs 0 = cs
step_n cs n =
  State.evalState (h cs n) 0
  where h :: ChipState -> Int -> State Int ChipState
        h cs 0 = return cs
        h cs n = do
          next_cs <- step cs
          Debug.Trace.trace (show cs) (h next_cs (n - 1))

step_end :: ChipState -> Int -> ChipState
step_end cs seed =
  State.evalState (h cs) seed
  where h :: ChipState -> State Int ChipState
        h cs = do
          next_cs <- step cs
          if next_cs == cs
          then return cs
          else h next_cs

print_screen :: ChipState -> String
print_screen cs =
  List.intercalate "\n" lines
  where s = screen cs
        lines = [line (s Boxed.! (i - 1)) | i <- [1..Boxed.length s]]
        line v = [if v Vector.! (i - 1) then '█' else ' '
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

print_registers :: ChipState -> String
print_registers cs = printf "PC: %8x, Rs: %sI: %8x"
                            (program_counter cs)
                            ((concatMap (printf "%02x, ") $ Vector.toList (registers cs)) :: String)
                            (address cs)

instance Show ChipState where
  show cs = List.intercalate "\n" [print_memory cs, print_registers cs, print_screen cs]

maze :: [Word16]
maze = [0x6000, 0x6100, 0xa222, 0xc201, 0x3201, 0xa21e, 0xd014, 0x7004,
        0x3040, 0x1204, 0x6000, 0x7104, 0x3120, 0x1204, 0x121c, 0x8040,
        0x2010, 0x2040, 0x8010]

-- https://archive.org/details/byte-magazine-1978-12/page/n113/mode/2up
print_ship :: [Word16]
print_ship = [0x6200, 0x6300, 0xa20a, 0xd236, 0x1208, 0x2070, 0x70f8, 0xd888]

-- https://archive.org/details/byte-magazine-1978-12/page/n121/mode/1up
rocket_game :: [Word16]
rocket_game =
  [0x6100, 0x6200, 0x6338, 0x641b, 0x6500, 0x6608, 0xa27e, 0xd563,
   0x226a, 0x4209, 0x1214, 0x7201, 0x681a, 0x6a00, 0xc71f, 0x770f,
   0x6900, 0xa278, 0xd786, 0xa27e, 0xd563, 0xc003, 0x8504, 0xd563,
   0x3f00, 0x1262, 0x600f, 0xe0a1, 0x6901, 0x3901, 0x1226, 0xf007,
   0x3000, 0x1226, 0xa278, 0xd786, 0x78FF, 0xd786, 0x3f00, 0x1262,
   0x6003, 0xf015, 0x3800, 0x1226, 0x226a, 0xa278, 0xd786, 0x81a4,
   0x1210, 0x6a01, 0x6003, 0xf018, 0x1258, 0xa2a0, 0xf133, 0xa2a2,
   0xf065, 0xf029, 0xd345, 0x00ee, 0x2070, 0x70f8, 0xd888, 0x7cd6,
   0x7c00]

loaded :: [Word16]
loaded | False = maze
       | False = print_ship
       | True = rocket_game

main :: IO ()
main = do
  let s0 = Main.init loaded
  let state = if False
              then step_n s0 1000
              else step_end s0 7
  putStrLn $ show state
