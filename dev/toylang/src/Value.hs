module Value (Value(..)) where

data Value =
    Int Integer
  | String String
  | Boolean Bool
  | Vector [Value]
  deriving (Show, Eq)
