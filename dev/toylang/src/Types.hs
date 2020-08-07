module Types
     (
       ParseTree(..)
     )where

data ParseTree =
    App [ParseTree]
  | Vector [ParseTree]
  | String String
  | Boolean Bool
  | Symbol String
  | Int Integer
  deriving (Eq, Show)
