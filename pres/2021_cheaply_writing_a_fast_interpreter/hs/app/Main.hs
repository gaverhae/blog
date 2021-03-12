module Main (main)
where

import Prelude hiding (exp,lookup)
--import Control.Monad (ap,liftM)
import Data.Map (Map)
import qualified Data.Map as Map

newtype Name = Name String
  deriving (Eq, Ord, Show)
newtype Value = Value Int
  deriving (Eq, Show)

data Exp where
  Lit :: Value -> Exp
  Var :: Name -> Exp
  Set :: Name -> Exp -> Exp
  Add :: Exp -> Exp -> Exp
  Sub :: Exp -> Exp -> Exp
  Mul :: Exp -> Exp -> Exp
  NotEq :: Exp -> Exp -> Exp
  Do :: [Exp] -> Exp
  While :: Exp -> Exp -> Exp
  Print :: Exp -> Exp
  deriving Show

neil :: Exp
neil =
  let x = Name "x"
      i = Name "i"
  in
  Do [
    Set x (Lit (Value 100)),
    Set i (Lit (Value 1000)),
    While (NotEq (Lit (Value 0)) (Var i))
      (Do [
        Set x (Add (Add (Add (Var x) (Lit (Value 4))) (Var x)) (Lit (Value 3))),
        Set x (Add (Add (Var x) (Lit (Value 2))) (Lit (Value 4))),
        Set i (Add (Lit (Value (-1))) (Var i))
      ]),
    Print $ Var x
    ]

fact :: Int -> Exp
fact x =
  let acc = Name "acc"
      i = Name "i"
  in
  Do [
    Set acc (Lit (Value 1)),
    Set i (Lit (Value x)),
    While (NotEq (Lit (Value 0)) (Var i))
      (Do [
        Set acc (Mul (Var acc) (Var i)),
        Set i (Sub (Var i) (Lit (Value 1))),
        Print (Var acc)
      ]),
    Print (Var acc)
  ]

sam :: Exp
sam =
  let x = Name "x"
  in
  Do [
    Set x (Lit (Value 13)),
    Print (Var x),
    Set x (Add (Var x) (Var x)),
    Print (Var x)
  ]

newtype Env = Env (Map Name Value)
  deriving Show
newtype OutputStream = OutputStream [Int]
  deriving Show

instance Semigroup OutputStream where
  (OutputStream a) <> (OutputStream b) = OutputStream (a <> b)

lookup :: Env -> Name -> Value
lookup (Env m) n = maybe undefined id (Map.lookup n m)

insert :: Env -> Name -> Value -> Env
insert (Env m) n v = Env $ Map.insert n v m

put :: OutputStream -> Value -> OutputStream
put (OutputStream os) (Value i) = OutputStream $ os ++ [i]

mt_out :: OutputStream
mt_out = OutputStream []

mt_env :: Env
mt_env = Env Map.empty

tree_walk_eval :: Exp -> (Value, OutputStream, Env)
tree_walk_eval ex =
  loop ex mt_out mt_env
  where
  loop :: Exp -> OutputStream -> Env -> (Value, OutputStream, Env)
  loop exp0 out0 env0 =
    case exp0 of
      Lit v -> (v, out0, env0)
      Var n -> (lookup env0 n, out0, env0)
      Set n exp1 -> let (v, out1, env1) = loop exp1 out0 env0
                    in (v, out1, insert env1 n v)
      Add e1 e2 -> do
        let (Value v1, out1, env1) = loop e1 out0 env0
        let (Value v2, out2, env2) = loop e2 out1 env1
        (Value (v1 + v2), out2, env2)
      Sub e1 e2 -> do
        let (Value v1, out1, env1) = loop e1 out0 env0
        let (Value v2, out2, env2) = loop e2 out1 env1
        (Value (v1 - v2), out2, env2)
      Mul e1 e2 -> do
        let (Value v1, out1, env1) = loop e1 out0 env0
        let (Value v2, out2, env2) = loop e2 out1 env1
        (Value (v1 * v2), out2, env2)
      NotEq e1 e2 -> do
        let (Value v1, out1, env1) = loop e1 out0 env0
        let (Value v2, out2, env2) = loop e2 out1 env1
        (Value $ if (v1 /= v2) then 1 else 0, out2, env2)
      Do (exps) -> foldl (\(_, out1, env1) exp1 -> loop exp1 out1 env1) (undefined, out0, env0) exps
      While condition body -> do
        let (Value c, out1, env1) = loop condition out0 env0
        if c == 1
        then do
          let (_, out2, env2) = loop body out1 env1
          loop (While condition body) out2 env2
        else (undefined, out1, env1)
      Print exp1 -> let (v, out1, env1) = loop exp1 out0 env0
                    in (v, put out1 v, env1)

add :: Value -> Value -> Value
add (Value a) (Value b) = Value (a + b)

sub :: Value -> Value -> Value
sub (Value a) (Value b) = Value (a - b)

not_eq :: Value -> Value -> Value
not_eq (Value a) (Value b) = Value $ if a /= b then 1 else 0

mul :: Value -> Value -> Value
mul (Value a) (Value b) = Value $ a * b

twe_cont :: Exp -> OutputStream
twe_cont e =
  loop e mt_env (\_ _ -> OutputStream [])
  where
  loop :: Exp -> Env -> (Env -> Value -> OutputStream) -> OutputStream
  loop exp0 env0 cont =
    let binop e1 e2 f = loop e1 env0 (\env1 v1 -> loop e2 env1 (\env2 v2 -> cont env2 (f v1 v2)))
    in
    case exp0 of
      Lit v -> cont env0 v
      Var n -> cont env0 (lookup env0 n)
      -- How can this work? :'(
      Print exp1 -> loop exp1 env0 (\env1 v -> put mt_out v <> cont env1 v)
      Set n exp1 -> loop exp1 env0 (\env1 v -> cont (insert env1 n v) v)
      Add e1 e2 -> binop e1 e2 add
      Sub e1 e2 -> binop e1 e2 sub
      Mul e1 e2 -> binop e1 e2 mul
      NotEq e1 e2 -> binop e1 e2 not_eq
      Do ([]) -> cont env0 undefined
      Do (exp1:[]) -> loop exp1 env0 (\env1 v -> cont env1 v)
      Do (exp1:exps) -> loop exp1 env0 (\env1 _ -> loop (Do exps) env1 (\env2 v -> cont env2 v))
      While condition body -> loop condition env0 (\env1 condition_value ->
        if (Value 1 == condition_value)
        then loop body env1 (\env2 _ ->
          loop (While condition body) env2 (\env3 v3 -> cont env3 v3))
        else cont env1 undefined)

{-
data EvalState = Map String Int

data EvalExec a where
  EvalBind :: EvalExec a -> (a -> EvalExec b) -> EvalExec b
  EvalReturn :: a -> EvalExec a
  EvalLookup :: String -> EvalExec Int
  EvalPrint :: Int -> EvalExec Int

instance Functor EvalExec where fmap = liftM
instance Applicative EvalExec where pure = return; (<*>) = ap
instance Monad EvalExec where return = EvalReturn; (>>=) = EvalBind

runEvalExec ::

treeWalkM :: Exp -> (Int, [Int], Map String Int)
treeWalkM e = runEvalExec

-}

main :: IO ()
main = do
  putStrLn "tree_walk_eval"
  print $ tree_walk_eval sam
  print $ tree_walk_eval $ fact 3
  print $ tree_walk_eval neil
  putStrLn "twe_cont"
  print $ twe_cont sam
  print $ twe_cont $ fact 3
  print $ twe_cont neil
  pure ()
