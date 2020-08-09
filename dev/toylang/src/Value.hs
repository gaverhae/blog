module Value where

import qualified Data.List

data Value =
    Int Integer
  | String String
  | Boolean Bool
  | Vector [Value]
  | Function String ([Value] -> Value)

primitivePlus :: Value
primitivePlus = Function "#<primitive>+" $ \args -> case args of
  [Int i, Int j] -> Int $ i + j
  _ -> error $ "invalid types for +:"  <> show args

instance Show Value where
  show (Int i) = show i
  show (String s) = "\"" <> s <> "\""
  show (Boolean True) = "true"
  show (Boolean False) = "false"
  show (Vector elems) = "[" <> concat (Data.List.intersperse " " (map show elems)) <> "]"
  show (Function s _) = s

instance Eq Value where
  (Int i) == (Int j) = i == j
  (String s) == (String t) = s == t
  (Boolean b) == (Boolean c) = b == c
  (Vector v) == (Vector w) = v == w
  (Function f _) == (Function g _) = f == g
