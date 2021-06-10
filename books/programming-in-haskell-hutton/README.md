# Programming in Haskell

My notes on reading [Programming in Haskell][0].

[0]: https://www.amazon.co.uk/Programming-Haskell-Graham-Hutton/dp/0521692695


# Chapter 6 - Recursive functions

## 6.1 - Basic concepts

```haskell
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

## 6.2 - Recursion on lists

```haskell
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
```

## 6.3 - Multiple arguments

```haskell
zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys
```

## 6.4 - Multiple recursion

```haskell
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)
```

## 6.5 - Mutual recursion

```haskell
even :: Int -> Bool
even 0 = True
even n = odd (n - 1)

odd :: Int -> Bool
odd 0 = False
odd n = even (n - 1)
```

## 6.6 - Advice on recursion

1. Define the type.
2. Enumerate the cases.
3. Define simple cases.
4. Define other cases.
5. Tidy up.

## 6.7 - Chapter remarks

:shrug:

## 6.8 - Exercises

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

# Chapter 7 - Higher-order functions

## 7.1 - Basic concepts

```haskell
twice :: (a -> a) -> a -> a
twice f x = f (f x)
```

## 7.2 - Processing lists

The function `map` applies a function to every element in a list:

```haskell
map :: (a -> b) -> [a] -> [b]
-- list comprehension
map f xs = [f x | x <- xs]
-- recursion
map f [] = []
map f (x:xs) = f x : map f xs
```

`filter` is also useful:

```haskell
filter :: (a -> Bool) -> [a] -> [a]
-- list comprehension
filter f xs = [x | x <- xs, f x]
-- recursion
filter f [] = []
filter f (x:xs) | f x = x : filter f xs
                | otherwise = filter f xs
```

A few others: `all`, `any`, `takeWhile`, `dropWhile`.

## 7.3 - The `foldr` function

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v [] = v
foldr f v (x:xs) = f x (foldr f v xs)
```

Note that if `f` is _lazy_ in its second argument, `foldr` may not need to
process the entire list and can thus be used on infinite lists.

## 7.4 - The `foldl` function

```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f v [] = v
foldl f v (x:xs) = foldl f (f v x) xs
```

As it is much rarer for `f` to be lazy in its first argument (and even then it
would still need to traverse the entire list), `foldl` tends to not work very
well with laziness.

## 7.5 - The composition operator

```haskell
(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)
```

## 7.6 - String transmitter

We simulate the transmission of a string of 0's and 1's. To simplify
conversions, we assume numbers are read right to left.

```haskell
-- Assumed to hold only either 0 or 1
newtype Bit = Bit Int

bin2int :: [Bit] -> Int
bin2int bits = sum [w * b | (w, (Bit b)) <- zip weights bits]
  where weights = iterate (* 2) 1

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = Bit (n `mod` 2) : int2bin (n `div` 2)
```

We want to work with 8-bit bytes:

```haskell
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat (Bit 0))
```

We can encode (& decode) characters:

```haskell
encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8
```

Finally, perfect transmission can be simulated with:

```haskell
transmit :: ([Bit] -> [Bit]) -> String -> String
transmit channel = decode . channel . encode

perfect_channel :: [Bit] -> [Bit]
perfect_channel = id
```

## 7.7 - Chapter remarks

More examples in [The Fun of
Programming](https://www.cs.ox.ac.uk/publications/books/fop/).

## 7.8 - Exercices

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

# Chapter 8 - Functional parsers

## 8.1 - Parser

A _parser_ is a program that takes a string of characters and procues some form
of tree.

## 8.2 - The parser type

A parser can naturally be viewed as a function from `String` to `tree`. There
are two issues with that, though: a parser may not consume the entire string,
so it is useful to return both a `tree` and a `String` with whatever parts of
the input were not processed, and a parser could fail, which we can represent
by returning a `Maybe`.

```haskell
newtype Parser = Parser (String -> Maybe (tree, String))
```

## 8.3 - Basic parsers

```haskell
return :: a -> Parser a
return v = Parser (\s -> Just (v, s))

failure :: Parser a
failure = Parser (\s -> Nothing)

item :: Parser Char
item = Parser (\s -> case s of
                       [] -> Nothing
                       (x:xs) -> Just (x, xs))

parse :: Parser a -> String -> Maybe (a, String)
parse (Parser f) = f
```

## 8.4 - Sequencing

```haskell
(>>=) :: Parser a -> (a -> Parser b) -> b
p >>= f = Parser (\s -> case parse p s of
                          Nothing -> Nothing
                          Just (v, out) -> parse (f v) out)
```

With `>>=` and `return`, we can use `do` notation. For example, a parser that
consumes three characters and returns the first and third as a pair could be
written:

```haskell
p :: Parser (Char, Char)
p = item >>= (\x ->
    item >>= (\_ ->
    item >>= (\y ->
    return (x, y))))
```

or, equivalently:

```haskell
p :: Parser (Char, Char)
p = do
  x <- item
  item
  y <- item
  return (x, y)
```

## 8.5 - Choice

```haskell
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\s -> case parse p s of
                          Nothing -> parse q s
                          success -> success)
```

## 8.6 - Derived primitives

```haskell
sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else failure

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do
  char x
  string xs
  return (x:xs)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do
  v <- p
  vs <- many p
  return (v:vs)

ident :: Parser String
ident = do
  x <- lower
  xs <- many alphanum
  return (x:xs)

nat :: Parser Int
nat = do
  xs <- many1 digit
  return (read xs)

space :: Parser ()
space = do
  many (sat isSpace)
  return ()
```

## 8.7 - Handling spacing

```haskell
token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)
```

## 8.8 - Arithmetic expressions

The following grammar:

```plaintext
expr ::= term (+ expr | e)
term ::= factor (* term | e)
factor ::= (expr) | nat
nat ::= 0 | 1 | 2 | ...
```

can be translated to:

```haskell
expr :: Parser Int
expr = do
  t <- term
  (    do symbol "+"; e <- expr; return (t + e)
   +++ do symbol "-"; e <- expr; return (t - e)
   +++ return t)

term :: Parser Int
term = do
  f <- factor
  (    do symbol "*"; t <- term; return (f * t)
   +++ do symbol "/"; t <- term; return (f `div` t)
   +++ return f)

factor :: Parser Int
factor = do
  e <- base
  (    do symbol "^"; f <- factor; return (e ^ f)
   +++ return e)
```

We can evaluate expressions of that grammar with:

```haskell
eval :: String -> Int
eval xs = case parse expr xs of
  Just (n, []) -> n
  Just (_, output) -> error $ "unused input " ++ output
  Nothing -> error "invalid input"
```

## 8.9 - Chapter notes

:shrug:

## 8.10 - Exercises

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
>
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

# Chapter 9 - Interactive programs

## 9.1 - Interaction

A _batch_ program knows all of its inputs at startup. By contrast, an
_interactive_ program can take additional input while it's running, as well as
produce incremental output.

## 9.2 - The IO type

In Haskell, an interactive program is a pure function that takes the "current
state of the world" as its argument, and produces a modified world as its
result. Like `Parser`, `IO` can return a result alongside the new world:

```haskell
type IO a = World -> (a, World)
```

## 9.3 - Basic actions

```haskell
getChar :: IO Char
-- native implementation

putChar :: Char -> IO ()
-- native implementation

return :: a -> IO a
-- native implementation
```

`return` provides a bridge from pure values to `IO` values. There is no way
back.

## 9.4 - Sequencing

```haskell
(>>=) :: IO a -> (a -> IO b) -> IO b
```

Thus we can use `do` notation.

## 9.5 - Derived primitives

```haskell
getLine :: IO String
getLine = do
  x <- getChar
  if x == '\n'
  then do
    return []
  else do
    xs <- getLine
    return (x:xs)

putStr :: String -> IO ()
putStr [] = return ()
putStr (x:xs) = do
  putChar x
  putStr xs

putStrLn :: String -> IO ()
putStrLn xs = do
  putStr xs
  putChar '\n'

beep :: IO ()
beep = putStr "\BEL"

cls :: IO ()
cls = putStr "\ESC[2J"

newtype Pos = (Int, Int)
  deriving Eq

goto :: Pos -> IO ()
goto (Pos (x, y)) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat (Pos p) xs = do
  goto p
  putStr xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a:as) = do
  a
  seqn as
```

## 9.6 - Calculator

```haskell

box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]

buttons :: [Char]
buttons = standard ++ extra
  where standard = "qcd=123+456-789*0()/"
        extra = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = seqn [ writeat (1, y) xs | (y, xs) <- zip [1..13] box]

display :: String -> IO ()
display xs = do
  writeat (3, 2) "             "
  writeat (3, 2) (reverse (take 13 (reverse xs)))

calc :: String -> IO ()
calc xs = do
  display xs
  c <- getCh
  if elem c buttons
  then do
    process c xs
  else do
    beep
    calc xs

process :: Char -> String -> IO ()
process c xs
  | elem c "qQ\ESC" = quit
  | elem c "dD\BS\DEL" = delete xs
  | elem c "=\n" = eval xs
  | elem c "cC" = clear
  | otherwise = press c xs

quit :: IO ()
quit = goto (1, 14)

delete :: String -> IO ()
delete " " = calc " "
delete xs = calc (init xs)

eval :: String -> IO ()
eval xs = case parse expr xs of
  [(n, " ")] -> calc (show n)
  _ -> do
     beep
     calc xs

clear :: IO ()
clear = calc " "

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do
  cls
  showbox
  clear
```

## 9.7 - Game of life

The game is played on a board; each cell is either alive of dead. The board is
assumed to wrap around. At each step:

- A living cell survives if it has 2 or 3 live neighbours.
- A dead cell comes back to life if it has exactly 3 live neighbours.

All cells update at the same time, and in all other cases the cell dies.

```haskell
width :: Int
width = 5

height :: Int
height = 5

newtype Board = Board [Pos]
  deriving Eq

glider :: Board
glider = Board [Pos (4, 2), Pos (2, 3), Pos (4, 3), Pos (3, 4), Pos (4, 4)]

showcells :: Board -> IO ()
showcells (Board b) = seqn [ writeat p "O" | p <- b ]

isAlive :: Board -> Pos -> Bool
isAlive (Board b) p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (Pos (x, y)) = map wrap [Pos (x - 1, y - 1), Pos (x, y - 1), Pos (x + 1, y - 1),
                                 Pos (x - 1, y),                     Pos (x + 1, y),
                                 Pos (x - 1, y + 1), Pos (x, y + 1), Pos (x + 1, y + 1)]

wrap :: Pos -> Pos
wrap (Pos (x, y)) = Pos (((x - 1) `mod` width) + 1,
                         ((y - 1) `mod` height) + 1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2, 3]]

births :: Board -> [Pos]
births b = [p | p <- rmdups (concat (map (neighbs b)))
              , isEmpty b p
              , liveneighbs b p == 3]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

nextgen :: Board -> Board
nextgen b = Board (survivors b ++ births b)

life :: Board -> IO ()
life b = do
  cls
  showcells b
  wait 5000
  life (nextgen b)

wait :: Int -> IO ()
wait n = seqn [ return () | _ <- [1..n]]
```

## 9.8 - Chapter remarks

:shrug:

## 9.9 - Exercises

> 1. Define an action `readLine :: IO String` that behaves in the same way as
>    `getLine`, except that it also permits the delete key to be used to remove
>    characters. Hint: the delete character is ’\DEL’, and the control string
>    for moving the cursor back one character is "\ESC[1D".

```haskell
readLine :: IO String
readLine = loop []
  where
  loop :: String -> IO String
  loop line = do
    c <- getCh
    case c of
      '\n' -> do
        putChar c
        return line
      '\DEL' -> do
        case line of
          [] -> loop []
          xs -> do
            putStr "\ESC[1D \ESC[1D"
            loop (init line)
      _ -> do
        putChar c
        loop (line ++ [c])
```
