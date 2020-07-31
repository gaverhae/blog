module Parser
    ( parse,
      ParseTree
    ) where

data ParseTree =
    Symbol String
  | Int Integer
  | Boolean Bool
  | App [ParseTree]
  | Vector [ParseTree]

parse :: String -> ParseTree
parse = undefined
