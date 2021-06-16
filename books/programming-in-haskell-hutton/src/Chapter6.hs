module Chapter6
where

import Prelude hiding (product, zip, odd, even, (^), and, concat, replicate, (!!), elem, take, sum, last)

-- # Chapter 6 - Recursive functions
--
-- ## 6.1 - Basic concepts
--
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- ## 6.2 - Recursion on lists

product :: Num a => [a] -> a
product [] = 1
product (x:xs) = x * product xs

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y = x : y : ys
                | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

-- ## 6.3 - Multiple arguments

zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys

-- ## 6.4 - Multiple recursion

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- ## 6.5 - Mutual recursion

even :: Int -> Bool
even 0 = True
even n = odd (n - 1)

odd :: Int -> Bool
odd 0 = False
odd n = even (n - 1)

-- ## 6.6 - Advice on recursion
--
-- 1. Define the type.
-- 2. Enumerate the cases.
-- 3. Define simple cases.
-- 4. Define other cases.
-- 5. Tidy up.
--
-- ## 6.7 - Chapter remarks
--
-- :shrug:
--
-- ## 6.8 - Exercises
--
-- > 1. Define the exponentiation operator `^` for non-negative integers using the
-- >    same pattern of recursion as the multiplication operator `*`, and show how
-- >    `2 ^ 3` is evaluated using your definition.

(^) :: Integral a => a -> a -> a
m ^ 0 = 1
m ^ n = m * m ^ (n - 1) -- ^ has higher precedence than *

-- 2 ^ 3 = 2 * (2 ^ 2)
--       = 2 * (2 * (2 ^ 1))
--       = 2 * (2 * (2 * (2 ^ 0)))
--       = 2 * (2 * (2 * 1))
--       = 2 * (2 * 2)
--       = 2 * 4
--       = 8
--
-- > 2. Using the definitions given in this chapter, show how `length [1, 2, 3]`,
-- >    `drop 3 [1, 2, 3, 4, 5]` and `init [1, 2, 3]` are evaluated.
--
-- length [1, 2, 3] = 1 + length [2, 3]
--                  = 1 + (1 + length [3])
--                  = 1 + (1 + (1 + length []))
--                  = 1 + (1 + (1 + 0))
--                  = 1 + (1 + 1)
--                  = 1 + 2
--                  = 3
--
-- drop 3 [1, 2, 3, 4, 5] = drop 2 [2, 3, 4, 5]
--                        = drop 1 [3, 4, 5]
--                        = drop 0 [4, 5]
--                        = [4, 5]
--
-- init [1, 2, 3] = 1 : init [2, 3]
--                = 1 : (2 : init [3])
--                = 1 : (2 : [])
--                = 1 : [2]
--                = [1, 2]
--
-- > 3. Without looking at the definition from the standard prelude, define the
-- >    following library functions using recursion:
-- >    - Decide if all logical values in a list are `True`:
-- >      and :: [Bool] -> Bool
-- >    - Concatenate a list of lists:
-- >      concat :: [[a]] -> [a]
-- >    - Produce a list with `n` identical elements:
-- >      replicate :: Int -> a -> [a]
-- >    - Select the nth element of a list:
-- >      (!!) :: [a] -> Int -> a
-- >    - Decide if a value is an element of a list:
-- >      elem :: Eq a => a -> [a] -> Bool
-- >    Note: most of these functions are in fact defined in the prelude using
-- >    other library functions, rather than explicit recursion.

and :: [Bool] -> Bool
and [] = True
and (False:_) = False
and (True:xs) = and xs

concat :: [[a]] -> [a]
concat [] = []
concat ([]:xss) = concat xss
concat ((x:xs):xss) = x : concat (xs:xss)

replicate :: Int -> a -> [a]
replicate n _ | n < 0 = error "would loop"
replicate 0 _ = []
replicate n x = x : replicate (n - 1) x

(!!) :: [a] -> Int -> a
[] !! _ = error "spec says this is not valid"
(x:xs) !! 0 = x
(x:xs) !! n = xs !! (n - 1)

elem :: Eq a => a -> [a] -> Bool
elem x [] = False
elem x (y:ys) | x == y = True
              | otherwise = elem x ys

-- > 4. Define a recursive function `merge :: Ord a => [a] -> [a] -> [a]` that
-- >    merges two sorted lists to give a single sorted list. For example:
-- >    ```haskell
-- >    > merge [2, 5, 6] [1, 3, 4]
-- >    [1, 2, 3, 4, 5, 6]
-- >    ```
-- >    Note: your definition should not use other functions on sorted lists such
-- >    as `insert` or `isort`, but should be defined using explicit recursion.

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- > 5. Using `merge`, define a recursive function `msort :: Ord a => [a] -> [a]`
-- >    that implements _merge sort_, in which the empty list and singleton lists
-- >    are already sorted, and any other list is sorted by merging together the
-- >    two lists that result from sorting the two halves of the list separately.
-- >
-- >    Hint: first define a function `halve :: [a] -> [([a], [a])] that splits a
-- >    list into two halves whose lengths differ by at most one.

halves :: [a] -> ([a], [a])
halves xs = (take n xs, drop n xs)
  where n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort h1) (msort h2)
  where (h1, h2) = halves xs

-- > 6. Using the five-step process, define the library functions that calculate
-- >    the `sum` of a list of numbers, `take` a given number of elements from the
-- >    start of a list, and select the `last` element of a non-empty list.

sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

take :: Int -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n _ | n < 0 = error "n must be positive"
take n (x:xs) = x : take (n - 1) xs

last :: [a] -> a
last [] = error "spec says non-empty list"
last [x] = x
last (x:xs) = last xs
