module Main where

import Prelude hiding (lookup)

import Control.Monad (ap,liftM)
import qualified Data.Map as Data (Map)
import qualified Data.Map

newtype Name = Name String
  deriving (Eq, Ord, Show)
newtype Value = Value Int
  deriving (Eq, Show)

data Op
  = Add
  | Sub
  | Mul
  | NotEq
  deriving Show

data Exp where
  Lit :: Value -> Exp
  Var :: Name -> Exp
  Set :: Name -> Exp -> Exp
  Bin :: Op -> Exp -> Exp -> Exp
  Do :: [Exp] -> Exp
  While :: Exp -> Exp -> Exp
  Print :: Exp -> Exp
  deriving Show

fact :: Int -> Exp
fact x =
  let acc = Name "acc"
      i = Name "i"
  in
  Do [
    Set acc (Lit (Value 1)),
    Set i (Lit (Value x)),
    While (Bin NotEq (Lit (Value 1)) (Var i))
      (Do [
        Set acc (Bin Mul (Var acc) (Var i)),
        Set i (Bin Sub (Var i) (Lit (Value 1))),
        Print (Var acc)
      ])
  ]

data TweIO = Output Int TweIO | Halt
  deriving Show

append :: TweIO -> Value -> TweIO
append Halt (Value v) = Output v Halt
append (Output p io) v = Output p (append io v)

newtype Env = Env (Data.Map Name Value)
  deriving Show

mt_env :: Env
mt_env = Env Data.Map.empty

lookup :: Env -> Name -> Value
lookup (Env m) n = maybe undefined id (Data.Map.lookup n m)

insert :: Env -> Name -> Value -> Env
insert (Env m) n v = Env $ Data.Map.insert n v m


bin :: Op -> Value -> Value -> Value
bin = \case
  Add -> \(Value a) (Value b) -> Value (a + b)
  Sub -> \(Value a) (Value b) -> Value (a - b)
  Mul -> \(Value a) (Value b) -> Value (a * b)
  NotEq -> \(Value a) (Value b) -> Value (if a /= b then 1 else 0)

tree_walk_eval :: Exp -> TweIO
tree_walk_eval ex =
  let (_, io, _) = loop ex Halt mt_env
  in io
  where
  loop :: Exp -> TweIO -> Env -> (Value, TweIO, Env)
  loop exp0 out0 env0 =
    case exp0 of
      Lit v -> (v, out0, env0)
      Var n -> (lookup env0 n, out0, env0)
      Set n exp1 -> let (v, out1, env1) = loop exp1 out0 env0
                    in (v, out1, insert env1 n v)
      Bin op e1 e2 ->
        let (v1, out1, env1) = loop e1 out0 env0
            (v2, out2, env2) = loop e2 out1 env1
        in ((bin op) v1 v2, out2, env2)
      Do (exps) -> foldl (\(_, out1, env1) exp1 -> loop exp1 out1 env1) (undefined, out0, env0) exps
      While condition body ->
        let (Value c, out1, env1) = loop condition out0 env0
        in if c == 1
           then do
             let (_, out2, env2) = loop body out1 env1
             loop (While condition body) out2 env2
           else (undefined, out1, env1)
      Print exp1 -> let (v, out1, env1) = loop exp1 out0 env0
                    in (v, append out1 v, env1)

data EvalExec a where
  EvalBind :: EvalExec a -> (a -> EvalExec b) -> EvalExec b
  EvalReturn :: a -> EvalExec a
  EvalPrint :: Value -> EvalExec ()
  EvalSet :: Name -> Value -> EvalExec ()
  EvalLookup :: Name -> EvalExec Value

instance Functor EvalExec where fmap = liftM
instance Applicative EvalExec where pure = return; (<*>) = ap
instance Monad EvalExec where return = EvalReturn; (>>=) = EvalBind

twe_mon :: Exp -> TweIO
twe_mon exp =
  let (_, io, _) = exec (mt_env, Halt) (eval exp)
  in io
  where
  eval :: Exp -> EvalExec Value
  eval = \case
    Lit v -> return v
    Var n -> do
      v <- EvalLookup n
      return v
    Set n exp -> do
      v <- eval exp
      EvalSet n v
      return v
    Bin op e1 e2 -> do
      v1 <- eval e1
      v2 <- eval e2
      return $ (bin op) v1 v2
    Do exps -> do
      mapM eval exps
      return undefined
    While condition body -> do
      c <- eval condition
      if Value 1 == c
      then do
        _ <- eval body
        eval (While condition body)
      else return undefined

    Print exp -> do
      v <- eval exp
      EvalPrint v
      return v

exec :: (Env, TweIO) -> EvalExec a -> (Env, TweIO, a)
exec (env_0, io_0) = \case
  EvalBind ma f -> let (env_1, io_1, a) = exec (env_0, io_0) ma
                   in exec (env_1, io_1) (f a)
  EvalReturn v -> (env_0, io_0, v)
  EvalLookup n -> (env_0, io_0, lookup env_0 n)
  EvalPrint v -> (env_0, append io_0 v, ())
  EvalSet n v -> (insert env_0 n v, io_0, ())

main :: IO ()
main = do
  print $ tree_walk_eval (fact 5)
  print $ twe_mon (fact 5)
