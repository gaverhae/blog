module Direct (eval) where

import qualified ParseTree
import qualified Value

data Env = Env [(String, Value.Value)]

defaultEnv :: Env
defaultEnv = Env [
  ("+", Value.primitivePlus)
  ]

specialForm :: String -> Bool
specialForm "cond" = True
specialForm _ = False

applySpecialForm :: String -> [ParseTree.ParseTree] -> Value.Value
applySpecialForm "cond" args =
  if length args `mod` 2 /= 0
  then
      error "invalid cond"
  else
      helper args
  where
    helper [] = error "at least one clause must match"
    helper (condition:consequence:rargs) =
      if (eval condition == Value.Boolean True)
      then
          eval consequence
      else
          helper rargs

eval :: ParseTree.ParseTree -> Value.Value
eval node = h defaultEnv node
  where
    h (Env env) node = case node of
      ParseTree.App [] -> error "invalid application"
      ParseTree.App (ParseTree.Symbol sf : args) | specialForm sf ->
        applySpecialForm sf args
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
