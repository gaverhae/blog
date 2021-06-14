module Chapter7
where

import Data.Char (chr, ord)

-- # Chapter 7 - Higher-order functions
--
-- ## 7.1 - Basic concepts

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- ## 7.2 - Processing lists
--
-- The function `map` applies a function to every element in a list:
--
-- ```haskell
-- map :: (a -> b) -> [a] -> [b]
-- -- list comprehension
-- map f xs = [f x | x <- xs]
-- -- recursion
-- map f [] = []
-- map f (x:xs) = f x : map f xs
-- ```
--
-- `filter` is also useful:
--
-- ```haskell
-- filter :: (a -> Bool) -> [a] -> [a]
-- -- list comprehension
-- filter f xs = [x | x <- xs, f x]
-- -- recursion
-- filter f [] = []
-- filter f (x:xs) | f x = x : filter f xs
--                 | otherwise = filter f xs
-- ```
--
-- A few others: `all`, `any`, `takeWhile`, `dropWhile`.
--
-- ## 7.3 - The `foldr` function
--
-- ```haskell
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f v [] = v
-- foldr f v (x:xs) = f x (foldr f v xs)
-- ```
--
-- Note that if `f` is _lazy_ in its second argument, `foldr` may not need to
-- process the entire list and can thus be used on infinite lists.
--
-- ## 7.4 - The `foldl` function
--
-- ```haskell
-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl f v [] = v
-- foldl f v (x:xs) = foldl f (f v x) xs
-- ```
--
-- As it is much rarer for `f` to be lazy in its first argument (and even then it
-- would still need to traverse the entire list), `foldl` tends to not work very
-- well with laziness.
--
-- ## 7.5 - The composition operator
--
-- ```haskell
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- f . g = \x -> f (g x)
-- ```
--
-- ## 7.6 - String transmitter
--
-- We simulate the transmission of a string of 0's and 1's. To simplify
-- conversions, we assume numbers are read right to left.

-- Assumed to hold only either 0 or 1

newtype Bit = Bit Int
  deriving (Show, Eq)

bin2int :: [Bit] -> Int
bin2int = foldr (\(Bit x) y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = Bit (n `mod` 2) : int2bin (n `div` 2)

-- We want to work with 8-bit bytes:

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat (Bit 0))

-- We can encode (& decode) characters:

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8


-- Finally, perfect transmission can be simulated with:

transmit :: ([Bit] -> [Bit]) -> String -> String
transmit channel = decode . channel . encode

perfect_channel :: [Bit] -> [Bit]
perfect_channel = id

-- ## 7.7 - Chapter remarks
--
-- More examples in [The Fun of
-- Programming](https://www.cs.ox.ac.uk/publications/books/fop/).
--
-- ## 7.8 - Exercices
--
-- > 1. Show how the list comprehension `[f x | x <- xs, p x]` can be re-expressed
-- >    using the higher-order functions `map` and `filter`.
--
-- ```haskell
-- map f (filter p xs)
-- ```
--
-- > 2. Without looking at the definitions from the standard prelude, define the
-- >    higher-order functions `all`, `any`, `takeWhile`, and `dropWhile`.

all_1 :: (a -> Bool) -> [a] -> Bool
all_1 _ [] = True
all_1 f (x:xs) | f x = all_1 f xs
               | otherwise = False

all_2 :: (a -> Bool) -> [a] -> Bool
all_2 f (x:xs) = f x && all_2 f xs

any_1 :: (a -> Bool) -> [a] -> Bool
any_1 _ [] = False
any_1 f (x:xs) = f x || any_1 f xs

any_2 f (x:xs) | f x = True
               | otherwise = any_2 f xs


takeWhile_1 :: (a -> Bool) -> [a] -> [a]
takeWhile_1 _ [] = []
takeWhile_1 f (x:xs) | f x = x : takeWhile_1 f xs
                     | otherwise = []

dropWhile_1 :: (a -> Bool) -> [a] -> [a]
dropWhile_1 _ [] = []
dropWhile_1 f (x:xs) | f x = dropWhile_1 f xs
                     | otherwise = xs

-- > 3. Redefine the functions `map f` and `filter p` using `foldr`.
--
-- ```haskell
-- map :: (a -> b) -> [a] -> [b]
-- map f = foldr (\elem acc -> f elem : acc) []
-- ```
--
-- ```haskell
-- filter (a -> Bool) -> [a] -> [a]
-- filter p = foldr (\elem acc -> [elem | f elem] ++ acc) []
-- {- or, more efficient
-- filter p = foldr (\elem acc -> if p elem then elem : acc else acc) []
-- -}
-- ```
--
-- > 4. Using `foldl`, define a function `dec2int :: [Int] -> Int` that converts a
-- >    decimal number into an integer. For example:
-- >    ```haskell
-- >    > dec2int [2, 3, 4, 5]
-- >    2345

dec2int :: [Int] -> Int
dec2int = foldl (\acc elem -> acc * 10 + elem) 0

-- > 5. Explain why the following definition is invalid:
-- >    ```haskell
-- >    sumsqreven = compose [sum, map (^2), filter even]
--
-- The type of compose is `[a -> a] -> (a -> a)`, so all the functions in the list
-- should return the same type as they accept. This works for `map (^2)` and
-- `filter even`, which can both be `[Int] -> [Int]`, but not for `sum`, as its
-- type is `[Num] -> Num`, and `[Num]` is not the same as `Num`.
--
-- Note that it "would work" in terms of the computations carried out, and the
-- type for `sumsqreven` would be correct, if we could define the type for
-- `compose` similarly to that of `.`.
--
-- > 6. Without looking at the standard prelude, define the higher-order library
-- >    function `curry` that converts a function on pairs into a curried
-- >    function, and, conversely, the function `uncurry` that converts a curried
-- >    function with two arguments into a function on pairs.
-- >
-- >    Hint: first write down the types of the two functions.

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y


-- > 7. A higher-order function `unfold` that encapsulates a simple pattern of
-- >    recursion for producing a list can be defined as follows:

unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

-- >    That is, the function `unfold p h t` produces the empty list if the
-- >    predicate `p` is true of the argument, and otherwise produces a non-empty
-- >    list by applying the function `h` to give the head, and the function `t`
-- >    to generate another argument that is recursively processed in the same way
-- >    to produce the tail of the list. For example, the function `int2bin` can
-- >    be rewritten more compactly using `unfold` as follows:
-- >    ```haskell
-- >    int2bin = unfold (== 0) (`mod` 2) (`div` 2)
-- >    ```
-- >    Redefine the functions `chop8`, `map f` and `iterate f` using `unfold`.

chop8_u :: [a] -> [[a]]
chop8_u = unfold null (take 8) (drop 8)

map_u :: (a -> b) -> [a] -> [b]
map_u f = unfold null (f . head) tail

iterate_u :: (a -> a) -> a -> [a]
iterate_u f = unfold (const False) id f

-- > 8. Modify the string transmitter program to detect simple transmission errors
-- >    using parity bits. That is, each eight-bit binary number produced during
-- >    encoding is extended with a parity bit, set to one if the number contains
-- >    an odd number of ones, and zero otherwise. In turn, each resulting
-- >    nine-bit binary number consumed during decoding is checked to ensure that
-- >    its parity bit is correct, with the parity bit being discarded if this is
-- >    the case, and a parity error reported otherwise.
-- >
-- >    Hint: the library function `error : String -> a` terminates evaluation and
-- >    displays the given string as an error message.

parity :: [Bit] -> Bit
parity bits = Bit $ sum (map (\(Bit c) -> c) bits) `mod` 2

add_parity :: [Bit] -> [Bit]
add_parity bits = parity bits : bits

encode_p :: String -> [Bit]
encode_p = concat . map (add_parity . make8 . int2bin . ord)

check_parity :: [Bit] -> [Bit]
check_parity (p:bits) =
  if parity (take 8 bits) == p then
    bits
  else
    error $ "byte " <> show bits <> " failed parity check"

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

decode_p :: [Bit] -> String
decode_p = map (chr . bin2int . check_parity) . chop9

transmit_perfect :: String -> String
transmit_perfect = decode . id . encode

transmit_lossy :: String -> String
transmit_lossy = decode . tail . encode

-- > 9. Test your new string transmitter program from the previous exercise using
-- >    a faulty communication channel that forgets the first bit, which can be
-- >    modelled using the `tail` function on lists of bits.
--
-- Surprise: it fails.
