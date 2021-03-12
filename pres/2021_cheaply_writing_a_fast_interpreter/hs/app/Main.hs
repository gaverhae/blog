module Main (main)
where

import Prelude hiding (exp,lookup)
import Control.Monad (ap,liftM)
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
data TweIO = Output Int TweIO | Halt
  deriving Show

bottom :: Value
bottom = undefined

lookup :: Env -> Name -> Value
lookup (Env m) n = maybe bottom id (Map.lookup n m)

insert :: Env -> Name -> Value -> Env
insert (Env m) n v = Env $ Map.insert n v m

put :: TweIO -> Value -> TweIO
put io (Value v) = Output v io

mt_env :: Env
mt_env = Env Map.empty

tree_walk_eval :: Exp -> (Value, TweIO, Env)
tree_walk_eval ex =
  loop ex Halt mt_env
  where
  append :: TweIO -> Value -> TweIO
  append Halt (Value v) = Output v Halt
  append (Output p io) v = Output p (append io v)
  loop :: Exp -> TweIO -> Env -> (Value, TweIO, Env)
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
      Do (exps) -> foldl (\(_, out1, env1) exp1 -> loop exp1 out1 env1) (bottom, out0, env0) exps
      While condition body -> do
        let (Value c, out1, env1) = loop condition out0 env0
        if c == 1
        then do
          let (_, out2, env2) = loop body out1 env1
          loop (While condition body) out2 env2
        else (bottom, out1, env1)
      Print exp1 -> let (v, out1, env1) = loop exp1 out0 env0
                    in (v, append out1 v, env1)

add :: Value -> Value -> Value
add (Value a) (Value b) = Value (a + b)

sub :: Value -> Value -> Value
sub (Value a) (Value b) = Value (a - b)

not_eq :: Value -> Value -> Value
not_eq (Value a) (Value b) = Value $ if a /= b then 1 else 0

mul :: Value -> Value -> Value
mul (Value a) (Value b) = Value $ a * b

twe_cont :: Exp -> TweIO
twe_cont e =
  loop e mt_env (\_ _ -> Halt)
  where
  loop :: Exp -> Env -> (Env -> Value -> TweIO) -> TweIO
  loop exp env cont =
    let binop e1 e2 f = loop e1 env (\env v1 -> loop e2 env (\env v2 -> cont env (f v1 v2)))
    in
    case exp of
      Lit v -> cont env v
      Var n -> cont env (lookup env n)
      -- How can this work? :'(
      Print exp -> loop exp env (\env v -> put (cont env v) v)
      Set n exp -> loop exp env (\env v -> cont (insert env n v) v)
      Add e1 e2 -> binop e1 e2 add
      Sub e1 e2 -> binop e1 e2 sub
      Mul e1 e2 -> binop e1 e2 mul
      NotEq e1 e2 -> binop e1 e2 not_eq
      Do ([]) -> cont env bottom
      Do (exp:[]) -> loop exp env (\env v -> cont env v)
      Do (exp:exps) -> loop exp env (\env _ -> loop (Do exps) env (\env v -> cont env v))
      While condition body -> loop condition env (\env condition_value ->
        if (Value 1 == condition_value)
        then loop body env (\env _ ->
          loop (While condition body) env (\env v -> cont env v))
        else cont env bottom)

data EvalExec a where
  EvalBind :: EvalExec a -> (a -> EvalExec b) -> EvalExec b
  EvalReturn :: a -> EvalExec a
  EvalLookup :: Name -> EvalExec Value
  EvalSet :: Name -> Value -> EvalExec ()
  EvalPrint :: Value -> EvalExec ()

instance Functor EvalExec where fmap = liftM
instance Applicative EvalExec where pure = return; (<*>) = ap
instance Monad EvalExec where return = EvalReturn; (>>=) = EvalBind

twe_mon :: Exp -> TweIO
twe_mon exp =
  exec (eval exp) mt_env (\_ _ -> Halt)
  where
  binop :: Exp -> Exp -> (Value -> Value -> Value) -> EvalExec Value
  binop e1 e2 f = do
    v1 <- eval e1
    v2 <- eval e2
    return $ f v1 v2
  eval :: Exp -> EvalExec Value
  eval = \case
    Lit v -> do
      return v
    Var n -> do
      v <- EvalLookup n
      return v
    Set n exp -> do
      v <- eval exp
      EvalSet n v
      return v
    Add e1 e2 -> binop e1 e2 add
    Sub e1 e2 -> binop e1 e2 sub
    Mul e1 e2 -> binop e1 e2 mul
    NotEq e1 e2 -> binop e1 e2 not_eq
    Do [] -> do
      return bottom
    Do [exp] -> do
      v <- eval exp
      return v
    Do (exp:exps) -> do
      _ <- eval exp
      eval (Do exps)
    While condition body -> do
      c <- eval condition
      if (Value 1) == c
      then do
        _ <- eval body
        eval (While condition body)
      else return bottom

    Print exp -> do
      v <- eval exp
      EvalPrint v
      return v

  exec :: EvalExec a -> Env -> (Env -> a -> TweIO) -> TweIO
  exec m env cont = case m of
    EvalBind prev step -> exec prev env (\env ret -> exec (step ret) env cont)
    EvalReturn v -> cont env v
    EvalLookup n -> cont env (lookup env n)
    EvalPrint v -> put (cont env ()) v
    EvalSet n v -> cont (insert env n v) ()

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
  putStrLn "twe_mon"
  print $ twe_mon sam
  print $ twe_mon $ fact 3
  print $ twe_mon neil
  pure ()
