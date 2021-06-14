# Programming in Haskell

My notes on reading [Programming in Haskell][0].

[0]: https://www.amazon.co.uk/Programming-Haskell-Graham-Hutton/dp/0521692695

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
