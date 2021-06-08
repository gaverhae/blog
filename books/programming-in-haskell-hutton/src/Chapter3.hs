module Chapter3
where

-- # Chapter 3 - Types and classes
-- 
-- ## 3.1 - Basic concepts
-- 
-- A _type_ is a collection of related values. `e :: t` means `e` is of type `t`.
-- Every expression in Haskell has a type.
-- 
-- ## 3.2 - Basic types
-- 
-- `Bool`, `Char`, `String` (which is really just `[Char]`), `Int` (fixed-length),
-- `Integer` (arbitrary precision), `Float`. Literal numbers can have multiple
-- types and are thus context-dependent.
-- 
-- ## 3.3 - List types
-- 
-- A _list_ `[a]` is a (possibly infinite) sequence of elements of the same type
-- `a`.
-- 
-- ## 3.4 - Tuple types
-- 
-- A _tuple_ is a finite sequence of components of possibly different types, for
-- example `(Bool, Int)`. The type conveys the number of elements as well as the
-- type of each position.
-- 
-- ## 3.5 - Function types
-- 
-- `a -> b` is the type of functions from `a` to `b`.
-- 
-- ## 3.6 - Curried functions
-- 
-- Function application associates to the left, so the type operator `->`
-- associates to the right.
-- 
-- ```haskell
-- add :: Int -> Int -> Int
-- add x y = x + y
-- ```
-- 
-- ## 3.7 - Polymorphic types
-- 
-- Lower-case letters in types denote type variables.
-- 
-- ## 3.8 - Overloaded types
-- 
-- Overloading is done through class constraints. Example:
-- 
-- ```haskell
-- (+) :: Num a => a -> a -> a
-- ```
-- 
-- ## 3.9 - Basic classes
-- 
-- `Eq` (`==`, `/=`), `Ord` (`<`, `>`, `>=`, `<=`, `min`, `max`), `Show` (`show`),
-- `Read` (`read`), `Num` (`+`, `-`, `*`, `negate`, `abs`, `signum`), `Integral`
-- (`div`, `mod`), `Fractional` (`/`, `recip`).
-- 
-- ## 3.10 - Chapter remarks
-- 
-- `Bool` comes from George Boole; "curried" comes from Haskell Curry.
-- 
-- ## 3.11 - Exercises
-- 
-- > 1. What are the types of the following values?
-- > ```haskell
-- > ['a', 'b', 'c']
-- > ('a', 'b', 'c')
-- > [(False , 'O'), (True , '1')]
-- > ([False, True], ['0', '1'])
-- > [tail, init, reverse]
-- > ```
-- 
exercise1 =
  let _ = ['a', 'b', 'c'] :: [Char]
      _ = ('a', 'b', 'c') :: (Char, Char, Char)
      _ = [(False , 'O'), (True , '1')] :: [(Bool, Char)]
      _ = ([False, True], ['0', '1']) :: ([Bool], [Char])
      _ = [tail, init, reverse] :: [[a] -> [a]]
  in ()

-- > 2. What are the types of the following functions?
-- > ```haskell
-- > second xs = head (tail xs)
-- > swap (x,y) = (y,x)
-- > pair x y = (x,y)
-- > double x = x * 2
-- > palindrome xs = reverse xs == xs
-- > twice f x = f (f x)
-- > ```
-- > Hint: take care to include the necessary class constraints if the functions
-- > are defined using overloaded operators.

second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x,y) = (y,x)

pair :: a -> b -> (a, b)
pair x y = (x,y)

double :: Num a => a -> a
double x = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- > 3. Check your answers to the preceding two questions using Hugs.
-- 
-- :shrug:
-- 
-- > 4. Why is it not feasible in general for function types to be instances of the
-- >   Eq class? When is it feasible? Hint: two functions of the same type are equal
-- >   if they always return equal results for equal arguments.
-- 
-- It could be feasible if we defined function equality as syntactic (i.e. literal
-- string) equality, or as alpha equivalence. However, as we (apparently) want to
-- define function equality based on external behaviour, checking two arbitrary
-- functions for equality would require either testing both of them for all
-- possible inputs (easy for input types like Bool or Int but harder for types
-- like Integer) or some sort of automated theorem prover that can test arbitrary
-- expressions for equivalence. Both approaches would result in very long calls to
-- (==), and neither could be guaranteeed correct for all types in the current
-- state of mathematical knowledge.
