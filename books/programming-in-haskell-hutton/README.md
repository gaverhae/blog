# Programming in Haskell

My notes on reading [Programming in Haskell][0].

[0]: https://www.amazon.co.uk/Programming-Haskell-Graham-Hutton/dp/0521692695

# Chapter 1

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

# Chapter 2

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
> non- empty list could be defined in terms of the library functions introduced
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


# Chapter 3

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
> twice f x = f(fx)
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
twice f x = f(fx)
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

# Chapter 4

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

> 1. Using a list comprehension, give an expression that calculates the sum 1^2
>   + 2^2 + 3^2 + ... + 100^2 of the first one hundred integer squares.

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
