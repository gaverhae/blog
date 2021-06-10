module Chapter5
where

import Prelude hiding (length)
import Data.Char (chr, isLower, isUpper, ord, toLower, toUpper)

-- # Chapter 5 - List comprehensions
--
-- ## 5.1 - Generators
--
-- In
--
-- ```haskell
-- [ x^2 | x <- [1 .. 5]]
-- ```
--
-- the symbol `|` is read "such that", the symbol `<-` is read "is drawn from",
-- and the expression `x <- [1 .. 5]` is called a _generator_.
--
-- Generators "nest" from left to right, i.e. the right-most "iterates faster":
--
-- ```haskell
-- [(x, y) | x <- [1, 2], y <- [4, 5]] == [(1, 4), (1, 5), (2, 4), (2, 5)]
-- ```
--
-- Examples:

concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]

firsts :: [(a, b)] -> [a]
firsts ps = [x | (x, _) <- ps]

length :: [a] -> Int
length xs = sum [1 | _ <- xs]

-- ## 5.2 - Guards
--
-- Like pattern matches, list comprehensions can contain _guards_, which filter
-- out non-matching values.

factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [2 .. n], prime x]

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

-- ## 5.3 - The `zip` function
--
-- ```haskell
-- > zip ['a', 'b', 'c'] [1, 2, 3, 4]
-- [('a', 1), ('b', 2), ('c', 3)]
-- ```

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0 .. n], x == x']
                 where n = length xs - 1

-- ## 5.4 - String comprehensions
--
-- `String` is `[Char]` so comprehensions work.

lowers :: String -> Int
lowers xs = length [x | x <- xs, isLower x]

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

-- ## 5.5 - The Caesar cipher
--
-- The Caesar cipher consists of shifting each letter three places further down in
-- the alphabet, with wrapping.

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

{-
shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c
-}

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

-- Cracking the Caesar cipher relies on letter frequencies.

table :: [Float]
table = [ 8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
--          a,   b,   c,   d,    e,   f,   g,   h,   i,   j,   k,   l,   m
          6.7, 7.5, 1.9, 0.1,  6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1 ]
--          n,   o,   p,   q,    r,   s,   t,   u,   v,   w,   x,   y,   z

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a' .. 'z']]
           where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e)^2)/e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (-factor) xs
  where factor = head (positions (minimum chitab) chitab)
        chitab = [chisqr (rotate n table') table | n <- [0 .. 25]]
        table' = freqs xs

-- ## 5.6 - Chatper remarks
--
-- List comprehensions can be formally defined in terms of underlying language
-- features; see the Haskell report for details.
--
-- ## 5.7 - Exercises
--
-- > 1. Using a list comprehension, give an expression that calculates the sum
-- >    `1^2 + 2^2 + 3^2 + ... + 100^2` of the first one hundred integer squares.
--
-- ```haskell
-- sum [ x^2 | x <- [1..100]]
-- ```
--
-- > 2. In a similar way to the function `length`, show how the library function
-- >    `replicate :: Int -> a -> [a]` that produces a list of identical elements
-- >    can be defined using a list comprehension. For example:
-- > ```haskell
-- > > replicate 3 True
-- > [True, True, True ]
-- > ```

replicate :: Int -> a -> [a]
replicate n a = [a | _ <- [1..n]]

-- > 3. A triple `(x, y, z)` of positive integers is _pythagorean_ if `x^2 + y^2 =
-- >  z^2`. Using a list comprehension, define a function `pyths :: Int ->
-- >  [(Int, Int, Int)]` that returns the list of all pythagorean triples whose
-- >  components are at most a given limit. For example:
-- > ```haskell
-- > > pyths 10
-- > [(3, 4, 5), (4, 3, 5), (6, 8, 10), (8, 6, 10)]
-- > ```

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(a, b, c) | c <- [1..n],
                       b <- [1..c],
                       a <- [1..c], -- note: 1..b if we know addition is commutative
                       a^2 + b^2 == c^2]

-- > 4. A positive integer is _perfect_ if it equals the sum of its factors,
-- >  excluding the number itself. Using a list comprehension and the function
-- >  `factors`, define a function `perfects :: Int -> [Int]` that returns the list
-- >  of all perfect numbers up to a given limit. For example:
-- > ```haskell
-- > > perfects 500
-- > [6, 28, 496]
-- > ```

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], 2 * x == sum(factors x)]

-- > 5. Show how the single comprehension `[(x,y) | x <- [1,2,3], y <- [4,5,6]]`
-- >  with two generators can be re-expressed using two comprehensions with
-- >  single generators. Hint: make use of the library function `concat` and
-- >  nest one comprehension within the other.
--
-- ```haskell
-- concat [[(x, y) | y <- [4, 5, 6]] | x <- [1, 2, 3]]
-- ```
--
-- > 6. Redefine the function `positions` using the function `find`.
--
-- ```haskell
-- old_positions x xs = [i | (x', i) <- zip xs [0..n], x == x']
--                      where n = length xs - 1
-- find k t = [v | (k', v) <- t, k == k']
--
-- positions :: Eq a => a -> [a] -> [Int]
-- positions x xs = find x (zip xs [0..])
-- ```
--
-- > 7. The scalar product of two lists of integers `xs` and `ys` of length `n` is
-- >  given by the sum of the products of corresponding integers. In a similar
-- >  manner to the function `chisqr`, show how a list comprehension can be used
-- >  to define a function `scalar_product :: [Int] -> [Int] -> Int` that
-- >  returns the scalar product of two lists. For example:
-- > ```haskell
-- > scalar_product [1, 2, 3] [4, 5, 6]
-- > 32
-- > ```

scalar_product :: [Int] -> [Int] -> Int
scalar_product xs ys = sum [x * y | (x, y) <- zip xs ys]

-- > 8. Modify the Caesar cipher program to also handle upper-case letters.

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | isUpper c = toUpper (shift n (toLower c))
          | otherwise = c
