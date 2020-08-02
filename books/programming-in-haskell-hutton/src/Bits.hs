module Bits where

import Data.Char (chr, ord)

newtype Bit = Bit Int
  deriving (Show, Eq)

bin2int :: [Bit] -> Int
bin2int = foldr (\(Bit x) y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = Bit (n `mod` 2) : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat (Bit 0))

parity :: [Bit] -> Bit
parity bits = Bit $ sum (map (\(Bit c) -> c) bits) `mod` 2

add_parity :: [Bit] -> [Bit]
add_parity bits = parity bits : bits

encode :: String -> [Bit]
encode = concat . map (add_parity . make8 . int2bin . ord)

check_parity :: [Bit] -> [Bit]
check_parity (p:bits) =
  if parity (take 8 bits) == p then
    bits
  else
    error $ "byte " <> show bits <> " failed parity check"

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int . check_parity) . chop9

transmit_perfect :: String -> String
transmit_perfect = decode . id . encode

transmit_lossy :: String -> String
transmit_lossy = decode . tail . encode
