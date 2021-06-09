module Chapter4
where

import Prelude hiding ((&&))

-- # Chapter 4 - Defining functions
--
-- ## 4.1 - New from old
--
isDigit :: Char -> Bool
isDigit c = (c >= '0') && (c <= '9')

even :: Integral a => a -> Bool
even n = n `mod` 2 == 0

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

recip :: Fractional a => a -> a
recip n = 1 / n

-- ## 4.2 - Conditional expressions
--
-- Conditional expressions must always have an `else`, and the two possible
-- results have to be of the same type.

abs :: Int -> Int
abs n = if n >= 0 then n else -n

signum :: Int -> Int
signum n = if n < 0
           then -1
           else if n == 0
                then 0
                else 1

-- ## 4.3 - Guarded equations
--
-- In this context, `|` is read "such that". Guard expressions are checked in
-- order.
--

abs_1 n | n >= 0 = n
        | otherwise = -n

signum_1 n | n < 0 = -1
           | n == 0 = 0
           | otherwise = 1

-- ## 4.4 - Pattern matching
--
-- Patterns are checked in order.
--
not :: Bool -> Bool
not False = True
not True = False

(&&) :: Bool -> Bool -> Bool
True && b = b
False && _ = False

fst :: (a, b) -> a
fst (x, _) = x

-- ## 4.5 - Lambda expressions

odds n = map (\x -> x * 2 + 1) [0 .. n - 1]

-- ## 4.6 - Sections
--
-- For any operator `+`, we get threes _sections_:
--
-- ```haskell
-- (+) :: a -> b -> c
-- (+) = \x -> \y -> x + y
--
-- (x +) :: b -> c
-- (x +) = \y -> x + y
--
-- (+ y) :: a -> c
-- (+ y) = \x -> x + y
-- ```
--
-- ## 4.7 - Chapter remarks
--
-- For the formal meaning of pattern matching, see the Haskell report. The `\` in
-- "lambda expressions" stands for the greek letter lambda, a reference to lambda
-- calculus, a model of (pure) computation that predates Turing machines.
--
-- ## 4.8 - Exercises
--
-- > 1. Using library functions, define a function
-- > ```haskell
-- > halve :: [a] -> ([a], [a])
-- > ```
-- > that splits an even-lengthed list into two halves. For example:
-- > ```haskell
-- > > halve [1, 2, 3, 4, 5, 6]
-- > ([1, 2, 3], [4, 5, 6])
-- > ```

halve :: [a] -> ([a], [a])
halve [] = ([],[])
halve xs | length xs == 2 * l = (take l xs, drop l xs)
         | otherwise = error "unspecified"
  where l = length xs `div` 2

-- > 2. Consider a function
-- > ```haskell
-- > safetail :: [a] -> [a]
-- > ```
-- > that behaves as the library function `tail`, except that `safetail` maps the
-- > empty list to itself, whereas `tail` produces an error in this case. Define
-- > `safetail` using:
-- >   (a) a conditional expression;
-- >   (b) guarded equations;
-- >   (c) pattern matching.
-- >   Hint: make use of the library function `null`.

safetail_a :: [a] -> [a]
safetail_a xs = if null xs then xs else tail xs

safetail_b :: [a] -> [a]
safetail_b xs | null xs = xs
              | otherwise = tail xs

safetail_c :: [a] -> [a]
safetail_c [] = []
safetail_c (x:xs) = xs

-- > 3. In a similar way to `&&`, show how the logical disjunction operator `||`
-- >    can be defined in four different ways using pattern matching.
--
-- ```haskell
-- (||) :: Bool -> Bool -> Bool
-- True || True = True
-- True || False = True
-- False || True = True
-- False || False = False
-- ```
--
-- ```haskell
-- (||) :: Bool -> Bool -> Bool
-- False || False = False
-- _ || _ = True
-- ```
--
-- ```haskell
-- (||) :: Bool -> Bool -> Bool
-- False || b = b
-- True || _ = True
-- ```
--
-- ```haskell
-- (||) :: Bool -> Bool -> Bool
-- a || b | a != b = True
--        | otherwise = a
-- ```
--
-- > 4. Redefine the following version of the conjunction operator using
-- >    conditional expressions rather than pattern matching:
-- > ```haskell
-- >   True && True = True
-- >   _ && _ = False
-- > ```
--
-- ```haskell
-- (&&) :: Bool -> Bool -> Bool
-- a && b = if a == True then
--            if b == True then
--              True
--            else False
--          else False
-- ```
--
-- > 5. Do the same for the following version, and note the difference in the number
-- >   of conditional expressions required:
-- > ```haskell
-- >   True && b = b
-- >   False && _ = False
-- > ```
--
-- ```haskell
-- (&&) :: Bool -> Bool -> Bool
-- a && b = if a == True then b
--          else False
-- ```
--
-- 2 vs 1 conditionals.
--
-- > 6. Show how the curried function definition
-- > ```haskell
-- >   mult x y z = x * y * z
-- > ```
-- >   can be understood in terms of lambda expressions.
--

mult :: Num a => a -> a -> a -> a
mult = \x -> \y -> \z -> x * y * z
