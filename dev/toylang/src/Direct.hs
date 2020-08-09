module Direct (eval) where

import qualified ParseTree
import qualified Value

data Env = Env [(String, Value.Value)]

defaultEnv :: Env
defaultEnv = Env [
  ("+", Value.primitivePlus)
  ]

eval :: ParseTree.ParseTree -> Value.Value
eval node = h defaultEnv node
  where
    h (Env env) node = case node of
      ParseTree.App [] -> error "invalid application"
      ParseTree.App (fn:args) -> case (eval fn, map eval args) of
        (Value.Function _ f, evargs) -> f evargs
        _ -> error "Not a function"
      ParseTree.Vector elems -> Value.Vector $ map eval elems
      ParseTree.String s -> Value.String s
      ParseTree.Boolean b -> Value.Boolean b
      ParseTree.Symbol s -> case lookup s env of
        Nothing -> error $ "undefined symbol: " <> s
        Just v -> v
      ParseTree.Int i -> Value.Int i
