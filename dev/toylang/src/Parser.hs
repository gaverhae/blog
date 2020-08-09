module Parser
    ( Parser.read
    ) where

import Data.Char (isSpace, isDigit)
import qualified ParseTree

newtype Parser a = Parser (String -> Maybe (a, String))

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

item :: Parser Char
item = Parser $ \case [] -> Nothing; (x:xs) -> Just (x, xs)

failure :: Parser a
failure = Parser $ \input -> Nothing

parse :: Parser a -> String -> Maybe (a, String)
parse (Parser f) = f

alt2 :: Parser a -> Parser a -> Parser a
alt2 p q = Parser $ \input -> case parse p input of
  Nothing -> parse q input
  res -> res

alts :: [Parser a] -> Parser a
alts [] = failure
alts (x:xs) = alt2 x (alts xs)

many :: Parser a -> Parser [a]
many p = alt2 (many1 p) (return [])

many1 :: Parser a -> Parser [a]
many1 p = do
  v <- p
  vs <- many p
  return (v:vs)

sat :: (Char -> Bool) -> Parser Char
sat pred = item >>= \x -> if pred x then return x else failure

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

char :: Char -> Parser Char
char x = token $ sat (== x)

literal :: String -> Parser String
literal [] = return []
literal (x:xs) = do
  char x
  literal xs
  return (x:xs)

(&&&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
p1 &&& p2 = \x -> p1 x && p2 x

isSymbolChar :: Char -> Bool
isSymbolChar = (not . isSpace) &&& (not . (`elem` ['(', ')', '[', ']']))

symbol :: Parser String
symbol = do
  h <- sat $ (not . isDigit) &&& isSymbolChar
  t <- many $ sat isSymbolChar
  return $ h:t

string :: Parser String
string = do
  char '"'
  s <- many no_unescaped_quote
  char '"'
  return s
  where no_unescaped_quote = do
          x <- item
          if x == '"' then
            failure
          else if x == '\\' then do
            y <- item
            return y
          else
            return x

int :: Parser Integer
int = do
  i <- many1 $ sat isDigit
  return $ Prelude.read i

read :: String -> ParseTree.ParseTree
read s = case parse form s of
  Nothing -> error "Parse failed"
  Just (p, []) -> p
  Just (p, (x:xs)) -> error $ "parse had leftover: \n" <> (x:xs) <> "\nafter parsing:\n" <> show p
  where form = alts [
          (do
            char '('
            forms <- token $ many form
            char ')'
            return $ ParseTree.App forms),
          (do
            char '['
            forms <- token $ many form
            char ']'
            return $ ParseTree.Vector forms),
          (do
            s <- token string
            return $ ParseTree.String s),
          (do
            token $ literal "true"
            return $ ParseTree.Boolean True),
          (do
            token $ literal "false"
            return $ ParseTree.Boolean False),
          (do
            s <- token symbol
            return $ ParseTree.Symbol s),
          (do
            n <- token int
            return $ ParseTree.Int n)]
