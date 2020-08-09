module Direct (eval) where

import qualified ParseTree
import qualified Value

newtype Env = Env [(String, Value.Value)]

defaultEnv :: Env
defaultEnv = Env [
  ("+", Value.primitivePlus)
  ]

specialForm :: String -> Bool
specialForm "cond" = True
specialForm _ = False

applySpecialForm :: String -> [ParseTree.ParseTree] -> Env -> Value.Value
applySpecialForm "cond" args env =
  if length args `mod` 2 /= 0
  then
      error "invalid cond"
  else
      helper args
  where
    helper [] = error "at least one clause must match"
    helper (condition:consequence:rargs) =
      if evalHelper env condition == Value.Boolean True
      then
          evalHelper env consequence
      else
          helper rargs

envLookup :: Env -> String -> Value.Value
envLookup (Env env) k = case lookup k env of
  Nothing -> error $ "undefined symbol: " <> k
  Just v -> v

evalHelper :: Env -> ParseTree.ParseTree -> Value.Value
evalHelper env node = case node of
    ParseTree.App [] -> error "invalid application"
    ParseTree.App (ParseTree.Symbol sf : args) | specialForm sf ->
      applySpecialForm sf args env
    ParseTree.App (fn:args) -> case (evalHelper env fn, map (evalHelper env) args) of
      (Value.Function _ f, evargs) -> f evargs
      _ -> error "Not a function"
    ParseTree.Vector elems -> Value.Vector $ map (evalHelper env) elems
    ParseTree.String s -> Value.String s
    ParseTree.Boolean b -> Value.Boolean b
    ParseTree.Symbol s -> envLookup env s
    ParseTree.Int i -> Value.Int i

eval :: ParseTree.ParseTree -> Value.Value
eval = evalHelper defaultEnv
