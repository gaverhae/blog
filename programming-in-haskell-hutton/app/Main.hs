module Main where

{-

# Chapter 1

1. Give another possible calculation for the result of double (double 2).

double (double 2) = double 2 + double 2
                  = 2 + 2    + double 2
                  = 2 + 2    + 2 + 2
                  = 2 + 4 + 2
                  = 2 + 6
                  = 8

2. Show that sum [x] = x for any number x.

sum [x] = x + sum []
        = x + 0
        = x

3. Define a function product that produces the product of a list of numbers,
  and show using your definition that product [ 2, 3, 4 ] = 24.

product [] = 1
product (x:xs) = x * product xs

product [2, 3, 4] = 2 * product [3, 4]
                  = 2 * 3 * product [4]
                  = 2 * 3 * 4 * product []
                  = 2 * 3 * 4 * 1
                  = 2 * 3 * 4
                  = 2 * 12
                  = 24

4. How should the definition of the function qsort be modified so that it
  produces a reverse sorted version of a list?

rqsort [] = []
rqsort (x:xs) = rqsort larger ++ [x] ++ rqsort smaller
                where smaller = [a | a <- xs, a <= x]
                      larger = [b | b <- xs, b > x]

5. What would be the effect of replacing ≤ by < in the definition of qsort?
   Hint: consider the example qsort [ 2, 2, 3, 1, 1 ].

Same as `sort | uniq` instead of just `sort`, i.e. the resulting list is sorted
and has no duplicates.

-}

{-

# Chapter 2

1. Parenthesise the following arithmetic expressions:

(2 ^ 3) * 4
(2 * 3) + (4 * 5)
2 + (3 * (4 ^ 5))

2. Work through the examples from this chapter using Hugs.

:shrug:

3. The script below contains three syntactic errors. Correct these errors and
  then check that your script works properly using Hugs.
  N = a 'div' length xs
      where a = 10
           xs = [1,2,3,4,5]

n = a `div` length xs
    where a = 10
          xs = [1,2,3,4,5]

4. Show how the library function last that selects the last element of a non-
  empty list could be defined in terms of the library functions introduced in
  this chapter. Can you think of another possible definition?

last xs = xs !! (length xs - 1)

last xs = head (drop (length xs - 1) xs)

last xs = head (reverse xs)

5. Show how the library function init that removes the last element from a
non-empty list could similarly be defined in two different ways.

init xs = take (length xs - 1) xs

init xs = reverse (tail (reverse xs))

-}

main :: IO ()
main = return ()
