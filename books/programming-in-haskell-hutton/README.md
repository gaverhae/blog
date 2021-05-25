# Programming in Haskell

My notes on reading [Programming in Haskell][0].

[0]: https://www.amazon.co.uk/Programming-Haskell-Graham-Hutton/dp/0521692695

# Chapter 1 - Introduction

## 1.1 - Functions

A _function_ is a mapping of one or more arguments to a single result. Example:

```haskell
double x = x + x
```

Function application is done by substituting definitions:

```haskell
  double 3
= -- definition of `double`
  3 + 3
= -- applying +
  6
```

In most cases, the order of substitutions does not matter. See Chapter 12 for
details.

## 1.2 - Functional programming

In the context of this book, _functional programming_ is a style of programming
in which the basic method of computation is the application of functions to
arguments. This is in contrast to _imperative programing_, in which the basic
method of computation is changing stored values.

## 1.3 - Features of Haskell

- Concise programs
- Powerful type system
- List comprehensions
- Recursive functions
- Higher-order functions
- Monadic effects
- Lazy evaluation
- Reasoning about programs

## 1.4 - Historical background

Haskell is the product of research and takes inspiration from languages before
it, among which Lisp, ML and Miranda.

## 1.5 - A taste of Haskell

```haskell
sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where smaller = [a | a <- xs, a <= x]
                     larger = [a | a <- xs, a > x]
```

## 1.6 - Chapter remarks

[Haskell 98](https://www.haskell.org/onlinereport/), [Haskell
2010](https://www.haskell.org/onlinereport/haskell2010/), [Hudak's take on
Haskell's history](https://dl.acm.org/doi/10.1145/72551.72554).

## 1.7 - Exercises

> 1. Give another possible calculation for the result of `double (double 2)`.

```haskell
double (double 2) = double 2 + double 2
                  = 2 + 2    + double 2
                  = 2 + 2    + 2 + 2
                  = 2 + 4 + 2
                  = 2 + 6
                  = 8
```

> 2. Show that `sum [x] = x` for any number `x`.

```haskell
sum [x] = x + sum []
        = x + 0
        = x
```

> 3. Define a function `product` that produces the product of a list of numbers,
> and show using your definition that `product [2, 3, 4] = 24`.

```haskell
product [] = 1
product (x:xs) = x * product xs
```

```haskell
product [2, 3, 4] = 2 * product [3, 4]
                  = 2 * 3 * product [4]
                  = 2 * 3 * 4 * product []
                  = 2 * 3 * 4 * 1
                  = 2 * 3 * 4
                  = 2 * 12
                  = 24
```

> 4. How should the definition of the function `qsort` be modified so that it
> produces a reverse sorted version of a list?

```haskell
rqsort [] = []
rqsort (x:xs) = rqsort larger ++ [x] ++ rqsort smaller
                where smaller = [a | a <- xs, a <= x]
                      larger = [b | b <- xs, b > x]
```

> 5. What would be the effect of replacing `<=` by `<` in the definition of `qsort`?
> Hint: consider the example `qsort [2, 2, 3, 1, 1]`.

Same as `sort | uniq` instead of just `sort`, i.e. the resulting list is sorted
and has no duplicates.

# Chapter 2 - First steps

## 2.1 - The Hugs system

This book is designed for Hugs but should work just as well with GHC.

## 2.2 - The standard prelude

`Prelude.hs` is loaded automatically. Operators have priorities. Basic
operations on integers: `+`, `-`, `*`, `div`, `^`. Useful functions on lists:
`head`, `tail`, `!!`, `take`, `drop`, `length`, `sum`, `product`, `++`,
`reverse`.

## 2.3 - Function application

Function application in Haskell is denoted by spacing, i.e. the math expression
$f(a,b)$ is written `f a b`; it has the highest priority and associates to the
left.

## 2.4 - Haskell scripts

Hugs can load files with `:l`. `:r` will reload all loaded scripts. Haskell is
generally indentation-sensitive, though blocks can also be explicit with `{}`
and `;`. Line comments start with `--` and nested comments can be delimited by
`{- -}`.

## 2.5 - Chapter remarks

See [haskell.org](https://www.haskell.org].

## 2.6 - Exercises

> 1. Parenthesise the following arithmetic expressions:
> ```
> 2 ^ 3 * 4
> 2 * 3 + 4 * 5
> 2 + 3 * 4 ^ 5
> ```

```
(2 ^ 3) * 4
(2 * 3) + (4 * 5)
2 + (3 * (4 ^ 5))
```

> 2. Work through the examples from this chapter using Hugs.

:shrug:

> 3. The script below contains three syntactic errors. Correct these errors and
> then check that your script works properly using Hugs.
> ```haskell
> N = a 'div' length xs
>   where a = 10
>        xs = [1,2,3,4,5]
> ```

```haskell
n = a `div` length xs
  where a = 10
        xs = [1,2,3,4,5]
```

> 4. Show how the library function `last` that selects the last element of a
> non-empty list could be defined in terms of the library functions introduced
> in this chapter. Can you think of another possible definition?

```haskell
last xs = xs !! (length xs - 1)
```

```haskell
last xs = head (drop (length xs - 1) xs)
```

```haskell
last xs = head (reverse xs)
```

> 5. Show how the library function `init` that removes the last element from a
> non-empty list could similarly be defined in two different ways.

```haskell
init xs = take (length xs - 1) xs
```

```haskell
init xs = reverse (tail (reverse xs))
````

# Chapter 3 - Types and classes

## 3.1 - Basic concepts

A _type_ is a collection of related values. `e :: t` means `e` is of type `t`.
Every expression in Haskell has a type.

## 3.2 - Basic types

`Bool`, `Char`, `String` (which is really just `[Char]`), `Int` (fixed-length),
`Integer` (arbitrary precision), `Float`. Literal numbers can have multiple
types and are thus context-dependent.

## 3.3 - List types

A _list_ `[a]` is a (possibly infinite) sequence of elements of the same type
`a`.

## 3.4 - Tuple types

A _tuple_ is a finite sequence of components of possibly different types, for
example `(Bool, Int)`. The type conveys the number of elements as well as the
type of each position.

## 3.5 - Function types

`a -> b` is the type of functions from `a` to `b`.

## 3.6 - Curried functions

Function application associates to the left, so the type operator `->`
associates to the right.

```haskell
add :: Int -> Int -> Int
add x y = x + y
```

## 3.7 - Polymorphic types

Lower-case letters in types denote type variables.

## 3.8 - Overloaded types

Overloading is done through class constraints. Example:

```haskell
(+) :: Num a => a -> a -> a
```

## 3.9 - Basic classes

`Eq` (`==`, `/=`), `Ord` (`<`, `>`, `>=`, `<=`, `min`, `max`), `Show` (`show`),
`Read` (`read`), `Num` (`+`, `-`, `*`, `negate`, `abs`, `signum`), `Integral`
(`div`, `mod`), `Fractional` (`/`, `recip`).

## 3.10 - Chapter remarks

`Bool` comes from George Boole; "curried" comes from Haskell Curry.

## 3.11 - Exercises

> 1. What are the types of the following values?
> ```haskell
> ['a', 'b', 'c']
> ('a', 'b', 'c')
> [(False , 'O'), (True , '1')]
> ([False, True], ['0', '1'])
> [tail, init, reverse]
> ```

```haskell
['a', 'b', 'c'] :: [Char]
('a', 'b', 'c') :: (Char, Char, Char)
[(False , 'O'), (True , '1')] :: [(Bool, Char)]
([False, True], ['0', '1']) :: ([Bool], [Char])
[tail, init, reverse] :: [[a] -> [a]]
```

> 2. What are the types of the following functions?
> ```haskell
> second xs = head (tail xs)
> swap (x,y) = (y,x)
> pair x y = (x,y)
> double x = x * 2
> palindrome xs = reverse xs == xs
> twice f x = f (f x)
> ```
> Hint: take care to include the necessary class constraints if the functions
> are defined using overloaded operators.

```haskell
second :: [a] -> a
second xs = head (tail xs)
```

```haskell
swap :: (a, b) -> (b, a)
swap (x,y) = (y,x)
```

```haskell
pair :: a -> b -> (a, b)
pair x y = (x,y)
```

```haskell
double :: Num a => a -> a
double x = x * 2
```

```haskell
palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs
```

```haskell
twice :: (a -> a) -> a -> a
twice f x = f (f x)
```

> 3. Check your answers to the preceding two questions using Hugs.

:shrug:

> 4. Why is it not feasible in general for function types to be instances of the
>   Eq class? When is it feasible? Hint: two functions of the same type are equal
>   if they always return equal results for equal arguments.

It could be feasible if we defined function equality as syntactic (i.e. literal
string) equality, or as alpha equivalence. However, as we (apparently) want to
define function equality based on external behaviour, checking two arbitrary
functions for equality would require either testing both of them for all
possible inputs (easy for input types like Bool or Int but harder for types
like Integer) or some sort of automated theorem prover that can test arbitrary
expressions for equivalence. Both approaches would result in very long calls to
(==), and neither could be guaranteeed correct for all types in the current
state of mathematical knowledge.

# Chapter 4 - Defining functions

## 4.1 - New from old

```haskell
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

even :: Integral a => a -> Bool
even n = n `mod` 2 == 0

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

recip :: Fractional a => a -> a
recip n = 1 / n
```

## 4.2 - Conditional expressions

Conditional expressions must always have an `else`, and the two possible
results have to be of the same type.

```haskell
abs :: Int -> Int
abs n = if n >= 0 then n else -n

signum :: Int -> Int
signum n = if n < 0
           then -1
           else if n == 0
                then 0
                else 1
```

## 4.3 - Guarded equations

In this context, `|` is read "such that". Guard expressions are checked in
order.

```haskell
abs n | n >= 0 = n
      | otherwise = -n

signum n | n < 0 = -1
         | n == 0 = 0
         | otherwise = 1
```

## 4.4 - Pattern matching

Patterns are checked in order.

```haskell
not :: Bool -> Bool
not False = True
not True = False

(&&) :: Bool -> Bool -> bool
True && b = b
False && _ = False

fst :: (a, b) -> a
fst (x, _) = x
```

## 4.5 - Lambda expressions

```haskell
odds n = map (\x -> x * 2 + 1) [0 .. n - 1]
```

## 4.6 - Sections

For any operator `+`, we get threes _sections_:

```haskell
(+) :: a -> b -> c
(+) = \x -> \y -> x + y

(x +) :: b -> c
(x +) = \y -> x + y

(+ y) :: a -> c
(+ y) = \x -> x + y
```

## 4.7 - Chapter remarks

For the formal meaning of pattern matching, see the Haskell report. The `\` in
"lambda expressions" stands for the greek letter lambda, a reference to lambda
calculus, a model of (pure) computation that predates Turing machines.

## 4.8 - Exercises

> 1. Using library functions, define a function
> ```haskell
> halve :: [a] -> ([a], [a])
> ```
> that splits an even-lengthed list into two halves. For example:
> ```haskell
> > halve [1, 2, 3, 4, 5, 6]
> ([1, 2, 3], [4, 5, 6])
> ```

```haskell
halve [] = []
halve xs | length xs == 2 * l = (take l xs, drop l xs)
         | otherwise = error "unspecified"
  where l = length xs `div` 2
```

> 2. Consider a function
> ```haskell
> safetail :: [a] -> [a]
> ```
> that behaves as the library function `tail`, except that `safetail` maps the
> empty list to itself, whereas `tail` produces an error in this case. Define
> `safetail` using:
>   (a) a conditional expression;
>   (b) guarded equations;
>   (c) pattern matching.
>   Hint: make use of the library function `null`.

```haskell
safetail xs = if null xs then xs else tail xs
```

```haskell
safetail xs | null xs = xs
            | otherwise = tail xs
```

```haskell
safetail [] = []
safetail (x:xs) = xs
```

> 3. In a similar way to `&&`, show how the logical disjunction operator `||`
>    can be defined in four different ways using pattern matching.

```haskell
(||) :: Bool -> Bool -> Bool
True || True = True
True || False = True
False || True = True
False || False = False
```

```haskell
(||) :: Bool -> Bool -> Bool
False || False = False
_ || _ = True
```

```haskell
(||) :: Bool -> Bool -> Bool
False || b = b
True || _ = True
```

```haskell
(||) :: Bool -> Bool -> Bool
a || b | a != b = True
       | otherwise = a
```

> 4. Redefine the following version of the conjunction operator using
>    conditional expressions rather than pattern matching:
> ```haskell
>   True && True = True
>   _ && _ = False
> ```

```haskell
(&&) :: Bool -> Bool -> Bool
a && b = if a == True then
           if b == True then
             True
           else False
         else False
```

> 5. Do the same for the following version, and note the difference in the number
>   of conditional expressions required:
> ```haskell
>   True && b = b
>   False && _ = False
> ```

```haskell
(&&) :: Bool -> Bool -> Bool
a && b = if a == True then b
         else False
```

2 vs 1 conditionals.

> 6. Show how the curried function definition
> ```haskell
>   mult x y z = x * y * z
> ```
>   can be understood in terms of lambda expressions.

```haskell
mult :: Num a => a -> a -> a -> a
mult = \x -> \y -> \z -> x * y * z
```

# Chapter 5

> 1. Using a list comprehension, give an expression that calculates the sum
>    `1^2 + 2^2 + 3^2 + ... + 100^2` of the first one hundred integer squares.

```haskell
sum [ x^2 | x <- [1..100]]
```

> 2. In a similar way to the function `length`, show how the library function
>    `replicate :: Int -> a -> [a]` that produces a list of identical elements
>    can be defined using a list comprehension. For example:
> ```haskell
> > replicate 3 True
> [True, True, True ]
> ```

```haskell
replicate :: Int -> a -> [a]
replicate n a = [a | _ <- [1..n]]
```

> 3. A triple `(x, y, z)` of positive integers is _pythagorean_ if `x^2 + y^2 =
>  z^2`. Using a list comprehension, define a function `pyths :: Int ->
>  [(Int, Int, Int)]` that returns the list of all pythagorean triples whose
>  components are at most a given limit. For example:
> ```haskell
> > pyths 10
> [(3, 4, 5), (4, 3, 5), (6, 8, 10), (8, 6, 10)]
> ```

```haskell
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(a, b, c) | c <- [1..n],
                       b <- [1..c],
                       a <- [1..c], -- note: 1..b if we know addition is commutative
                       a^2 + b^2 == c^2]
```

> 4. A positive integer is _perfect_ if it equals the sum of its factors,
>  excluding the number itself. Using a list comprehension and the function
>  `factors`, define a function `perfects :: Int -> [Int]` that returns the list
>  of all perfect numbers up to a given limit. For example:
> ```haskell
> > perfects 500
> [6, 28, 496]
> ```

```haskell
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], 2 * x == sum(factors x)]
```

> 5. Show how the single comprehension `[(x,y) | x <- [1,2,3], y <- [4,5,6]]`
>  with two generators can be re-expressed using two comprehensions with
>  single generators. Hint: make use of the library function `concat` and
>  nest one comprehension within the other.

```haskell
concat [[(x, y) | y <- [4, 5, 6]] | x <- [1, 2, 3]]
```

> 6. Redefine the function `positions` using the function `find`.

```haskell
old_positions x xs = [i | (x', i) <- zip xs [0..n], x == x']
                     where n = length xs - 1
find k t = [v | (k', v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])
```

> 7. The scalar product of two lists of integers `xs` and `ys` of length `n` is
>  given by the sum of the products of corresponding integers. In a similar
>  manner to the function `chisqr`, show how a list comprehension can be used
>  to define a function `scalar_product :: [Int] -> [Int] -> Int` that
>  returns the scalar product of two lists. For example:
> ```haskell
> scalar_product [1, 2, 3] [4, 5, 6]
> 32
> ```

```haskell
scalar_product :: [Int] -> [Int] -> Int
scalar_product xs ys = sum [x * y | (x, y) <- zip xs ys]
```

> 8. Modify the Caesar cipher program to also handle upper-case letters.

```haskell
shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c
```

must be replaced with

```haskell
shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | isUpper c = toUpper (shift n (toLower c))
          | otherwise = c
```
# Chapter 6

> 1. Define the exponentiation operator `^` for non-negative integers using the
>    same pattern of recursion as the multiplication operator `*`, and show how
>    `2 ^ 3` is evaluated using your definition.

```haskell
(^) :: Integral a => a -> a -> a
m ^ 0 = 1
m ^ (n + 1) = m * m ^ n -- ^ has higher precedence than *
```

```haskell
2 ^ 3 = 2 * (2 ^ 2)
      = 2 * (2 * (2 ^ 1))
      = 2 * (2 * (2 * (2 ^ 0)))
      = 2 * (2 * (2 * 1))
      = 2 * (2 * 2)
      = 2 * 4
      = 8
```

> 2. Using the definitions given in this chapter, show how `length [1, 2, 3]`,
>    `drop 3 [1, 2, 3, 4, 5]` and `init [1, 2, 3]` are evaluated.

```haskell
length [1, 2, 3] = 1 + length [2, 3]
                 = 1 + (1 + length [3])
                 = 1 + (1 + (1 + length []))
                 = 1 + (1 + (1 + 0))
                 = 1 + (1 + 1)
                 = 1 + 2
                 = 3
```

```haskell
drop 3 [1, 2, 3, 4, 5] = drop 2 [2, 3, 4, 5]
                       = drop 1 [3, 4, 5]
                       = drop 0 [4, 5]
                       = [4, 5]
```

```haskell
init [1, 2, 3] = 1 : init [2, 3]
               = 1 : (2 : init [3])
               = 1 : (2 : [])
               = 1 : [2]
               = [1, 2]
```

> 3. Without looking at the definition from the standard prelude, define the
>    following library functions using recursion:
>    - Decide if all logical values in a list are `True`:
>      ```haskell
>      and :: [Bool] -> Bool
>      ```
>    - Concatenate a list of lists:
>      ```haskell
>      concat :: [[a]] -> [a]
>      ```
>    - Produce a list with `n` identical elements:
>      ```haskell
>      replicate :: Int -> a -> [a]
>      ```
>    - Select the nth element of a list:
>      ```haskell
>      (!!) :: [a] -> Int -> a
>      ```
>    - Decide if a value is an element of a list:
>      ```haskell
>      elem :: Eq a => a -> [a] -> Bool
>      ```
>    Note: most of these functions are in fact defined in the prelude using
>    other library functions, rather than explicit recursion.

```haskell
and :: [Bool] -> Bool
and [] = True
and (False:_) = False
and (True:xs) = and xs
```

```haskell
concat :: [[a]] -> [a]
concat [] = []
concat ([]:xss) = concat xss
concat ((x:xs):xss) = x : concat (xs:xss)
```

```haskell
replicate :: Int -> a -> [a]
replicate n _ | n < 0 = error "would loop"
replicate 0 _ = []
replicate n x = x : replicate (n - 1) x
```

```haskell
(!!) :: [a] -> Int -> a
[] !! _ = error "spec says this is not valid"
(x:xs) !! 0 = x
(x:xs) !! n = xs !! (n - 1)
```

```haskell
elem :: Eq a => a -> [a] -> Bool
elem x [] = False
elem x (y:ys) | x == y = True
              | otherwise = elem x ys
```

> 4. Define a recursive function `merge :: Ord a => [a] -> [a] -> [a]` that
>    merges two sorted lists to give a single sorted list. For example:
>    ```haskell
>    > merge [2, 5, 6] [1, 3, 4]
>    [1, 2, 3, 4, 5, 6]
>    ```
>    Note: your definition should not use other functions on sorted lists such
>    as `insert` or `isort`, but should be defined using explicit recursion.

```haskell
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys
```

> 5. Using `merge`, define a recursive function `msort :: Ord a => [a] -> [a]`
>    that implements _merge sort_, in which the empty list and singleton lists
>    are already sorted, and any other list is sorted by merging together the
>    two lists that result from sorting the two halves of the list separately.
>
>    Hint: first define a function `halve :: [a] -> [([a], [a])] that splits a
>    list into two halves whose lengths differ by at most one.

```haskell
halves :: [a] -> ([a], [a])
halves xs = (take n xs, drop n xs)
  where n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort h1) (msort h2)
  where (h1, h2) = halves xs
```

> 6. Using the five-step process, define the library functions that calculate
>    the `sum` of a list of numbers, `take` a given number of elements from the
>    start of a list, and select the `last` element of a non-empty list.

```haskell
sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs
```

```haskell
take :: Int -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n _ | n < 0 = error "n must be positive"
take n (x:xs) = x : take (n - 1) xs
```

```haskell
last :: [a] -> a
last [] = error "spec says non-empty list"
last [x] = x
last (x:xs) = last xs
```

# Chapter 7

> 1. Show how the list comprehension `[f x | x <- xs, p x]` can be re-expressed
>    using the higher-order functions `map` and `filter`.

```haskell
map f (filter p xs)
```

> 2. Without looking at the definitions from the standard prelude, define the
>    higher-order functions `all`, `any`, `takeWhile`, and `dropWhile`.

```haskell
all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all f (x:xs) | f x = all f xs
             | otherwise = False
{- or
all f (x:xs) = f x && all f xs
-}
```

```haskell
any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any f (x:xs) = f x || any f xs
{- or
any f (x:xs) | f x = True
             | otherwise = any f xs
-}
```

```haskell
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile f (x:xs) | f x = x : takeWhile f xs
                   | otherwise = []
```

```haskell
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile f (x:xs) | f x = dropWhile f xs
                   | otherwise = xs
```

> 3. Redefine the functions `map f` and `filter p` using `foldr`.

```haskell
map :: (a -> b) -> [a] -> [b]
map f = foldr (\elem acc -> f elem : acc) []
```

```haskell
filter (a -> Bool) -> [a] -> [a]
filter p = foldr (\elem acc -> [elem | f elem] ++ acc) []
{- or, more efficient
filter p = foldr (\elem acc -> if p elem then elem : acc else acc) []
-}
```

> 4. Using `foldl`, define a function `dec2int :: [Int] -> Int` that converts a
>    decimal number into an integer. For example:
>    ```haskell
>    > dec2int [2, 3, 4, 5]
>    2345

```haskell
dec2int :: [Int] -> Int
dec2int = foldl (\acc elem -> acc * 10 + elem) 0
```

> 5. Explain why the following definition is invalid:
>    ```haskell
>    sumsqreven = compose [sum, map (^2), filter even]

The type of compose is `[a -> a] -> (a -> a)`, so all the functions in the list
should return the same type as they accept. This works for `map (^2)` and
`filter even`, which can both be `[Int] -> [Int]`, but not for `sum`, as its
type is `[Num] -> Num`, and `[Num]` is not the same as `Num`.

Note that it "would work" in terms of the computations carried out, and the
type for `sumsqreven` would be correct, if we could define the type for
`compose` similarly to that of `.`.

> 6. Without looking at the standard prelude, define the higher-order library
>    function `curry` that converts a function on pairs into a curried
>    function, and, conversely, the function `uncurry` that converts a curried
>    function with two arguments into a function on pairs.
>
>    Hint: first write down the types of the two functions.

```haskell
curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)
```

```haskell
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y
```

> 7. A higher-order function `unfold` that encapsulates a simple pattern of
>    recursion for producing a list can be defined as follows:
>    ```haskell
>    unfold p h t x | p x = []
>                   | otherwise = h x : unfold p h t (t x)
>    ```
>    That is, the function `unfold p h t` produces the empty list if the
>    predicate `p` is true of the argument, and otherwise produces a non-empty
>    list by applying the function `h` to give the head, and the function `t`
>    to generate another argument that is recursively processed in the same way
>    to produce the tail of the list. For example, the function `int2bin` can
>    be rewritten more compactly using `unfold` as follows:
>    ```haskell
>    int2bin = unfold (== 0) (`mod` 2) (`div` 2)
>    ```
>    Redefine the functions `chop8`, `map f` and `iterate f` using `unfold`.

```haskell
chop8 :: [a] -> [[a]]
chop8 = unfold null (take 8) (drop 8)
```

```haskell
map :: (a -> b) -> [a] -> [b]
map f = unfold null (f . head) tail
```

```haskell
iterate :: (a -> a) -> a -> [a]
iterate f = unfold (const False) id f
```

> 8. Modify the string transmitter program to detect simple transmission errors
>    using parity bits. That is, each eight-bit binary number produced during
>    encoding is extended with a parity bit, set to one if the number contains
>    an odd number of ones, and zero otherwise. In turn, each resulting
>    nine-bit binary number consumed during decoding is checked to ensure that
>    its parity bit is correct, with the parity bit being discarded if this is
>    the case, and a parity error reported otherwise.
>
>    Hint: the library function `error : String -> a` terminates evaluation and
>    displays the given string as an error message.

See [`Bits.hs`](src/Bits.hs).

> 9. Test your new string transmitter program from the previous exercise using
>    a faulty communication channel that forgets the first bit, which can be
>    modelled using the `tail` function on lists of bits.

Surprise: it fails.

# Chapter 8

> 1. The [library file](http://www.cs.nott.ac.uk/~pszgmh/Code.zip) also defines
>    a parser `int :: Parser Int` for an integer. Without looking at this
>    definition, define `int`. Hint: an integer is either a minus symbol
>    followed by a natural number, or a natural number.

```
int :: Parser Int
int = do
  sign <- (do
    symbol "-"
    return (-1)) +++ return 1
  n <- natural
  return $ sign * n
```

> 2. Define a parser `comment :: Parser ()` for ordinary Haskell comments that
>    begin with the symbol `--` and extend to the end of the current line,
>    which is represented by the control character `\n`.

```
comment :: Parser ()
comment = do
  symbol "--"
  many (sat (/= '\n'))
  char '\n'
  return ()
```

> 3. Using our second grammar for arithmetic expressions, draw the two possible
>    parse trees for the expression `2 + 3 + 4`.

```
[expr: [expr: [term: [factor: [nat: '2']]]]
       '+'
       [expr: [expr: [term: [factor: [nat: '3']]]]
              '+'
              [expr: [term: [factor: [nat: '4']]]]]]
```

or

```
[expr: [expr: [expr: [term: [factor: [nat: 2]]]]
              +
              [expr: [term: [factor: [nat: 3]]]]]
       +
       [expr: [term: [factor: [nat: 4]]]]]
```

> 4. Using our third grammar for arithmetic expressions, draw the parse trees
>    for the expressions `2 + 3`, `2 * 3 * 4` and `(2 + 3) + 4`.

```
[expr: [term: [factor: [nat: '2']]]
       '+'
       [expr: [term: [factor: [nat: '3']]]]]
```

```
[expr: [term: [factor: [:nat '2']]
              '*'
              [term: [factor: [nat: '3']]
                     '*'
                     [term: [factor: [nat: '4']]]]]]
```

```
[expr: [term: [factor: '('
                       [expr: [term: [factor: [nat: '2']]]
                               '+'
                               [expr: [term: [factor: [nat: '3']]]]]
                       ')']]
       '+'
       [expr: [term: [factor: [nat: '4']]]]]
```

> 5. Explain why the final simplification of the grammar for arithmetic
>    expressions has a dramatic effect on the efficiency of the resulting
>    parser. Hint: begin by considering how an expression comprising a single
>    number would be parsed if this step had not been made.

Before the simplification, the first rule reads as:

```
expr ::= term '+' expr | term
```

A naive translation of that rule would result in duplicated work. In the case
of a single natural number, the program would first parse the number as a term
(expr -> term -> factor -> nat), then check for the presence of a `+` sign, see
that it's missing, fail the parse, move on to the next alternative, and parse
the number again as, this time, a term followed by nothing.

Superficially this may look like doing the work twice, which is not that bad.
However, consider that this applies to every single level in the tree, and to
each expression, hence the simplification changes the complexity class of the
parser.

> 6. Extend the parser for arithmetic expressions to support subtraction and
>    division, based upon the following extensions to the grammar:
>   ```
>   expr ::= term ('+' expr | '-' expr | ε)
>   term ::= factor ('*' term | '/' term | ε)
>   ```

Changing the layout a bit to make the change more visible, we need to change:

```haskell
expr :: Parser Int
expr = do
  t <- term
  (    do symbol "+"; e <- expr; return (t + e)
   +++ return t)
```

to

```haskell
expr :: Parser Int
expr = do
  t <- term
  (    do symbol "+"; e <- expr; return (t + e)
   +++ do symbol "-"; e <- expr; return (t - e)
   +++ return t)
```

and similaly for `term`. See [`Parser.hs`](src/Parser.hs).

Note that, whereas before this change the choice of associating to the right
was purely an optimization (with multiplication and additon both being
associative and commutative), with the introduction of integer division this
choice now has a semantic implication. We decide to ignore it here as this is
about parsing.

> 7. Further extend the grammar and parser for arithmetic expressions to
>    support exponentiation, which is assumed to associate to the right and
>    have higher priority than multiplication and division, but lower priority
>    than parentheses and numbers. For example, `2 ^ 3 * 4` means `(2 ^ 3) *
>    4`. Hint: the new level of priority requires a new rule in the grammar.

New grammar:

```
expr ::= term ('+' expr | '-' expr | ε)
term ::= factor ('*' term | ε)
factor ::= base ('^' factor | ε)
base ::= '(' expr ')' | nat
nat ::= '0' | '1' | '2' | ...
```

New code: add a new parser `exp` and change the existing `factor` to be:

```
factor :: Parser Int
factor = do
  e <- base
  (    do symbol "^"; f <- factor; return (e ^ f)
   +++ return e)

exp :: Parser Int
exp = (do symbol "("; e <- expr; symbol ")"; return e) +++ natural
```

see [`Parser.hs`](src/Parser.hs) for details.

> 8. Consider expressions built up from natural numbers using a subtraction
>    operator that is assumed to associate to the left.
>    a. Define a natural [🤮] grammar for such expressions.

"natural" has no meaning in the context of a constructed world of abstract
representations. I'll read it as _naïve_, i.e. one that hits the issue hinted
at in _c_.

```
expr = expr ('-' nat | ε)
nat = '0' | '1' | ...
```

>    b. Translate this grammar into a parser `expr :: Parser Int`.

I'll choose a name we have not already overloaded five times in this chapter,
though. Following the pattern we have used to implement the right-associative
ones, we could try:

```haskell
negexpr :: Parser Int
negexpr = do
  e <- negexpr
  (    do symbol "-"; n <- natural; return (e - n)
   +++ return e)
```

>    c. What is the problem with this parser?

In trying to associate to the left (in a naïve way), we end up with a recursive
function that _does not terminate_. It would if our parsing code was "smarter",
i.e. if it explored all the possible parse trees in a breadth-first rather than
depth-first way, but as it stands the grammar has no way to stop on even the
simplest argument of a single natural number.

>    d. Show how it can be fixed. Hint: rewrite the parser using the repetition
>       primitive `many` and the library function `foldl`.

This would realize our intention:

```haskell
negexpr :: Parser Int
negexpr = do
  first <- natural
  others <- many (do symbol "-"; n <- natural; return n)
  return $ foldl (-) first others
```

though the mapping to the grammar as written is a lot less direct. If we had
some sort of repetition syntax in our grammar, say using _*_, we could rewrite
the grammar to match as something like:

```
negexpr ::= nat ('-' nat)*
nat ::= '0' | '1' | ...
```

though this would not express the desired associativity.
