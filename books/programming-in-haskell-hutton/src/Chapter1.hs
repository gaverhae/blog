module Chapter1
where

import Prelude hiding (sum, product)

-- # Chapter 1 - Introduction
--
-- ## 1.1 - Functions
--
-- A _function_ is a mapping of one or more arguments to a single result.
-- Example:

double x = x + x

-- Function application is done by substituting definitions:
--
-- ```haskell
--   double 3
-- = -- definition of `double`
--   3 + 3
-- = -- applying +
--   6
-- ```
--
-- In most cases, the order of substitutions does not matter. See Chapter 12
-- for details.
--
-- ## 1.2 - Functional programming
--
-- In the context of this book, _functional programming_ is a style of
-- programming in which the basic method of computation is the application of
-- functions to arguments. This is in contrast to _imperative programing_, in
-- which the basic method of computation is changing stored values.
--
-- ## 1.3 - Features of Haskell
--
-- - Concise programs
-- - Powerful type system
-- - List comprehensions
-- - Recursive functions
-- - Higher-order functions
-- - Monadic effects
-- - Lazy evaluation
-- - Reasoning about programs
--
-- ## 1.4 - Historical background
--
-- Haskell is the product of research and takes inspiration from languages
-- before it, among which Lisp, ML and Miranda.
--
-- ## 1.5 - A taste of Haskell
--
sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where smaller = [a | a <- xs, a <= x]
                     larger = [a | a <- xs, a > x]

-- ## 1.6 - Chapter remarks
--
-- [Haskell 98](https://www.haskell.org/onlinereport/), [Haskell
-- 2010](https://www.haskell.org/onlinereport/haskell2010/), [Hudak's take on
-- Haskell's history](https://dl.acm.org/doi/10.1145/72551.72554).
--
-- ## 1.7 - Exercises
--
-- > 1. Give another possible calculation for the result of `double (double 2)`.
--
-- ```haskell
-- double (double 2) = double 2 + double 2
--                   = 2 + 2    + double 2
--                   = 2 + 2    + 2 + 2
--                   = 2 + 4 + 2
--                   = 2 + 6
--                   = 8
-- ```
--
-- > 2. Show that `sum [x] = x` for any number `x`.
--
-- ```haskell
-- sum [x] = x + sum []
--         = x + 0
--         = x
-- ```
--
-- > 3. Define a function `product` that produces the product of a list of numbers,
-- > and show using your definition that `product [2, 3, 4] = 24`.

product [] = 1
product (x:xs) = x * product xs

-- ```haskell
-- product [2, 3, 4] = 2 * product [3, 4]
--                   = 2 * 3 * product [4]
--                   = 2 * 3 * 4 * product []
--                   = 2 * 3 * 4 * 1
--                   = 2 * 3 * 4
--                   = 2 * 12
--                   = 24
-- ```
--
-- > 4. How should the definition of the function `qsort` be modified so that it
-- > produces a reverse sorted version of a list?
--

rqsort [] = []
rqsort (x:xs) = rqsort larger ++ [x] ++ rqsort smaller
                where smaller = [a | a <- xs, a <= x]
                      larger = [b | b <- xs, b > x]

-- > 5. What would be the effect of replacing `<=` by `<` in the definition of `qsort`?
-- > Hint: consider the example `qsort [2, 2, 3, 1, 1]`.
--
-- Same as `sort | uniq` instead of just `sort`, i.e. the resulting list is sorted
-- and has no duplicates.
