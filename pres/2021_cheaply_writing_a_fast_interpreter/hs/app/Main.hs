module Main (main)
where

import Prelude hiding (exp,lookup)
import qualified Control.Exception
import Control.Monad (ap,liftM)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text.Lazy.Builder
import Data.Traversable (for)
import qualified Formatting
import qualified Formatting.Clock
import qualified Formatting.Formatters
import qualified System.Clock

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

direct :: Int
direct =
  loop 100 1000
  where
  loop :: Int -> Int -> Int
  loop x i = if (0 == i)
             then x
             else
             let x1 = x + 4 + x +3
             in let x2 = x1 + 2 + 4
             in loop x2 (i - 1)

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

append :: TweIO -> Value -> TweIO
append Halt (Value v) = Output v Halt
append (Output p io) v = Output p (append io v)

mt_env :: Env
mt_env = Env Map.empty

tree_walk_eval :: Exp -> TweIO
tree_walk_eval ex =
  let (_, io, _) = loop ex Halt mt_env in io
  where
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

closure_eval :: Exp -> TweIO
closure_eval e =
  let (_, _, io) = compile e (mt_env, Halt) in io
  where
  binop :: Exp -> Exp -> (Value -> Value -> Value) -> (Env, TweIO) -> (Value, Env, TweIO)
  binop e1 e2 op =
      let f1 = compile e1
          f2 = compile e2
      in \(env, io) ->
        let (v1, env1, io1) = f1 (env, io)
            (v2, env2, io2) = f2 (env1, io1)
        in (op v1 v2, env2, io2)
  compile :: Exp -> (Env, TweIO) -> (Value, Env, TweIO)
  compile = \case
    Lit v -> \(env, io) -> (v, env, io)
    Var n -> \(env, io) -> (lookup env n, env, io)
    Set n exp ->
      let f = compile exp
      in \(env, io) ->
        let (v1, env1, io1) = f (env, io)
        in (v1, insert env1 n v1, io1)
    Add e1 e2 -> binop e1 e2 add
    Sub e1 e2 -> binop e1 e2 sub
    Mul e1 e2 -> binop e1 e2 mul
    NotEq e1 e2 -> binop e1 e2 not_eq
    Do [] -> \(env, io) -> (bottom, env, io)
    Do [exp] -> let f = compile exp in \(env, io) -> f (env, io)
    Do exps ->
      let fs = foldr (\exp cont -> let f = compile exp
                                   in \_ (env, io) ->
                                     let (v1, env1, io1) = f (env, io)
                                     in cont v1 (env1,io1))
                     (\v (env, io) -> (v, env, io))
                     exps
      in \(env, io) -> fs bottom (env, io)
    While condition body ->
      let cond = compile condition
          bod = compile body
          loop = \(env, io) ->
            let (c, env1, io1) = cond (env, io)
            in if (Value 1) == c
               then let (_, env2, io2) = bod (env1, io1)
                    in loop (env2, io2)
               else (bottom, env1, io1)
      in loop
    Print exp ->
      let f = compile exp
      in \(env, io) ->
        let (v1, env1, io1) = f (env, io)
        in (v1, env1, append io1 v1)

{-
closure_cont :: Exp -> TweIO
closure_cont e =
  (compile e (\_ _ -> Halt)) mt_env
  where
  binop :: Exp -> Exp -> (Value -> Value -> Value) -> ((Env -> Value) -> Env -> TweIO) -> Env -> TweIO
  binop e1 e2 f cont = undefined e1 e2 f cont
  compile :: Exp -> ((Env -> Value) -> Env -> TweIO) -> Env -> TweIO
  compile exp cont = case exp of
    Lit v -> cont (\_ -> v)
    Var n -> cont (\env -> lookup env n)
    Set n exp -> compile exp (\f -> \env -> k
    Add e1 e2 -> binop e1 e2 add cont
    Sub e1 e2 -> binop e1 e2 sub cont
    Mul e1 e2 -> binop e1 e2 mul cont
    NotEq e1 e2 -> binop e1 e2 not_eq cont
    Do [] -> undefined
    Do [exp] -> undefined exp
    Do (exp:exps) -> undefined exp exps
    While condition body -> undefined condition body
    Print exp -> undefined exp
-}


main :: IO ()
main = do
  let switch = 1
  if switch == (0::Int)
  then do
    _ <- for [("tree_walk_eval", tree_walk_eval)
             ,("twe_cont", twe_cont)
             ,("twe_mon", twe_mon)
             ,("closure_eval", closure_eval)
             --,("closure_cont", closure_cont)
             ]
             (\(n, f) -> do
               putStrLn n
               print $ f sam
               print $ f $ fact 3
               print $ f neil)
    pure()
  else do
    let now = System.Clock.getTime System.Clock.Monotonic
    let raw_string = Formatting.now . Data.Text.Lazy.Builder.fromString
    let printDur = Formatting.fprint
                     (Formatting.Formatters.string
                      Formatting.%
                      raw_string ": "
                      Formatting.%
                      Formatting.Clock.timeSpecs
                      Formatting.%
                      raw_string "\n")
    let bench s f = do
          start <- now
          _ <- Control.Exception.evaluate f
          end <- now
          printDur s start end
    bench "direct" (direct)
    bench "direct" (direct)
    bench "tree_walk_eval" (tree_walk_eval neil)
    bench "twe_cont" (twe_cont neil)
    bench "twe_mon" (twe_mon neil)
    bench "closure_eval" (closure_eval neil)
    pure ()
  pure ()
