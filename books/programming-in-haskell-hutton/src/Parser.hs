module Parser where

import Data.Char (isDigit, isLower, isUpper, isAlpha, isAlphaNum, isSpace)

newtype Parser a = Parser (String -> Maybe (a, String))

failure :: Parser a
failure = Parser $ \input -> Nothing

item :: Parser Char
item = Parser $ \case [] -> Nothing; (x:xs) -> Just (x, xs)

parse :: Parser a -> String -> Maybe (a, String)
parse (Parser f) = f

instance Functor Parser where
  fmap f p = Parser $ \input -> case parse p input of
    Nothing -> Nothing
    Just (x, s) -> Just (f x, s)

instance Applicative Parser where
  pf <*> pa = Parser $ \input -> case parse pf input of
    Nothing -> Nothing
    Just (f, s) -> case parse pa s of
      Nothing -> Nothing
      Just (a, t) -> Just (f a, t)
  pure v = Parser $ \input -> Just (v, input)

instance Monad Parser where
  p >>= f = Parser $ \input -> case parse p input of
    Nothing -> Nothing
    Just (v, output) -> parse (f v) output

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser $ \input -> case parse p input of
  Nothing -> parse q input
  res -> res

sat :: (Char -> Bool) -> Parser Char
sat pred = item >>= \x -> if pred x then return x else failure

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

base :: Parser Int
base = (do
  symbol "("
  e <- expr
  symbol ")"
  return e) +++ natural

eval :: String -> Int
eval xs = case parse expr xs of
  Just (n, []) -> n
  Just (_, output) -> error $ "unused input " ++ output
  Nothing -> error "invalid input"

negexpr :: Parser Int
negexpr = do
  first <- natural
  others <- many (do symbol "-"; n <- natural; return n)
  return $ foldl (-) first others
