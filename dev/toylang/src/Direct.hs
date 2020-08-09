module Direct (eval) where

import qualified ParseTree
import qualified Value

eval :: ParseTree.ParseTree -> Value.Value
eval node = case node of
  ParseTree.App [] -> error "invalid application"
  ParseTree.App (fn:args) -> undefined
  ParseTree.Vector elems -> Value.Vector $ map eval elems
  ParseTree.String s -> Value.String s
  ParseTree.Boolean b -> Value.Boolean b
  ParseTree.Symbol s -> undefined
  ParseTree.Int i -> Value.Int i
