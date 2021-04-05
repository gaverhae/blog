module Main (main)
where

import Prelude hiding (exp,lookup)
import Control.DeepSeq (NFData)
import qualified Control.DeepSeq
import qualified Control.Exception
import Control.Monad (ap,liftM,void)
import qualified Data.Map as Data (Map)
import qualified Data.Map
import qualified Data.Text.Lazy.Builder
import Data.Traversable (for)
import qualified Data.Vector as Data (Vector)
import qualified Data.Vector
import qualified Formatting
import qualified Formatting.Clock
import qualified Formatting.Formatters
import GHC.Generics (Generic)
import qualified System.Clock

newtype Name = Name String
  deriving (Eq, Ord, Show)
newtype Value = Value Int
  deriving (Eq, Show)
  deriving newtype Num

data Op
  = Add
  | Sub
  | Mul
  | NotEq
  deriving (Show)

data Exp where
  Lit :: Value -> Exp
  Var :: Name -> Exp
  Set :: Name -> Exp -> Exp
  Bin :: Op -> Exp -> Exp -> Exp
  Do :: [Exp] -> Exp
  While :: Exp -> Exp -> Exp
  Print :: Exp -> Exp
  deriving Show

bin :: Op -> Value -> Value -> Value
bin = \case
  Add -> (+)
  Sub -> (-)
  Mul -> (*)
  NotEq -> \v1 v2 -> if v1 /= v2 then 1 else 0

neil :: Exp
neil =
  let x = Name "x"
      i = Name "i"
      t = Name "t"
  in
  Do [
    Set x (Lit 100),
    Set i (Lit 1000),
    While (Bin NotEq (Lit 0) (Var i))
      (Do [
        Set x (Bin Add (Bin Add (Bin Add (Var x) (Lit 4)) (Var x)) (Lit 3)),
        Set x (Bin Add (Bin Add (Var x) (Lit 2)) (Lit 4)),
        Set i (Bin Add (Lit (-1)) (Var i))
      ]),
    Print $ Var x,
    Print $ Var t
    ]

direct :: Env -> TweIO
direct _ =
  loop 100 1000
  where
  loop :: Int -> Int -> TweIO
  loop x0 i =
    if (0 == i)
    then put Halt (Value x0)
    else
    let x1 = x0 + 4 + x0 + 3
        x2 = x1 + 2 + 4
    in loop x2 (i - 1)

fact :: Int -> Exp
fact x =
  let acc = Name "acc"
      i = Name "i"
  in
  Do [
    Set acc (Lit 1),
    Set i (Lit (Value x)),
    While (Bin NotEq (Lit 0) (Var i))
      (Do [
        Set acc (Bin Mul (Var acc) (Var i)),
        Set i (Bin Sub (Var i) (Lit 1)),
        Print (Var acc)
      ]),
    Print (Var acc)
  ]

sam :: Exp
sam =
  let x = Name "x"
  in
  Do [
    Set x (Lit 13),
    Print (Var x),
    Set x (Bin Add (Var x) (Var x)),
    Print (Var x)
  ]

newtype Env = Env (Data.Map Name Value)
  deriving Show
data TweIO = Output Int TweIO | Halt
  deriving (Show, Generic, NFData)

bottom :: Value
bottom = undefined

lookup :: Env -> Name -> Value
lookup (Env m) n = maybe bottom id (Data.Map.lookup n m)

insert :: Env -> Name -> Value -> Env
insert (Env m) n v = Env $ Data.Map.insert n v m

put :: TweIO -> Value -> TweIO
put io (Value v) = Output v io

append :: TweIO -> Value -> TweIO
append Halt (Value v) = Output v Halt
append (Output p io) v = Output p (append io v)

mt_env :: Env
mt_env = Env Data.Map.empty

tree_walk_eval :: Exp -> Env -> TweIO
tree_walk_eval ex env =
  let (_, io, _) = loop ex Halt env in io
  where
  loop :: Exp -> TweIO -> Env -> (Value, TweIO, Env)
  loop exp0 out0 env0 =
    case exp0 of
      Lit v -> (v, out0, env0)
      Var n -> (lookup env0 n, out0, env0)
      Set n exp1 -> let (v, out1, env1) = loop exp1 out0 env0
                    in (v, out1, insert env1 n v)
      Bin op e1 e2 -> do
        let (v1, out1, env1) = loop e1 out0 env0
        let (v2, out2, env2) = loop e2 out1 env1
        ((bin op) v1 v2, out2, env2)
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

twe_cont :: Exp -> Env -> TweIO
twe_cont e env =
  loop e env (\_ _ -> Halt)
  where
  loop :: Exp -> Env -> (Env -> Value -> TweIO) -> TweIO
  loop exp env cont =
    case exp of
      Lit v -> cont env v
      Var n -> cont env (lookup env n)
      Print exp -> loop exp env (\env v -> put (cont env v) v)
      Set n exp -> loop exp env (\env v -> cont (insert env n v) v)
      Bin op e1 e2 -> loop e1 env (\env v1 -> loop e2 env (\env v2 -> cont env ((bin op) v1 v2)))
      Do ([]) -> cont env bottom
      Do (exp:[]) -> loop exp env (\env v -> cont env v)
      Do (exp:exps) -> loop exp env (\env _ -> loop (Do exps) env (\env v -> cont env v))
      While condition body -> loop condition env (\env condition_value ->
        if (1 == condition_value)
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

twe_mon :: Exp -> Env -> TweIO
twe_mon exp env =
  exec (eval exp) env (\_ _ -> Halt)
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
    Do [] -> return bottom
    Do [exp] -> eval exp
    Do (exp:exps) -> do
      _ <- eval exp
      eval (Do exps)
    While condition body -> do
      c <- eval condition
      if 1 == c
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

closure_eval :: Exp -> Env -> TweIO
closure_eval e =
  let c = compile e in \env -> let (_, _, io) = c (env, Halt) in io
  where
  compile :: Exp -> (Env, TweIO) -> (Value, Env, TweIO)
  compile = \case
    Lit v -> \(env, io) -> (v, env, io)
    Var n -> \(env, io) -> (lookup env n, env, io)
    Set n exp ->
      let f = compile exp
      in \(env, io) ->
        let (v1, env1, io1) = f (env, io)
        in (v1, insert env1 n v1, io1)
    Bin op e1 e2 ->
      let f1 = compile e1
          f2 = compile e2
      in \(env, io) ->
        let (v1, env1, io1) = f1 (env, io)
            (v2, env2, io2) = f2 (env1, io1)
        in ((bin op) v1 v2, env2, io2)
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
            in if 1 == c
               then let (_, env2, io2) = bod (env1, io1)
                    in loop (env2, io2)
               else (bottom, env1, io1)
      in loop
    Print exp ->
      let f = compile exp
      in \(env, io) ->
        let (v1, env1, io1) = f (env, io)
        in (v1, env1, append io1 v1)

closure_cont :: Exp -> Env -> TweIO
closure_cont e =
  let f = compile e (\f (env, io) ->
        let (_, env1, io1) = f (env, io)
        in (env1, io1))
  in \env ->
    let (_, io) = f (env, Halt)
    in io
  where
  compile :: Exp -> (((Env, TweIO) -> (Value, Env, TweIO)) -> (Env, TweIO) -> (Env, TweIO)) -> (Env, TweIO) -> (Env, TweIO)
  compile exp cont = case exp of
    Lit v -> cont (\(env, io) -> (v, env, io))
    Var n -> cont (\(env, io) -> (lookup env n, env, io))
    Set n exp -> compile exp (\f ->
      cont (\(env, io) ->
        let (v, env1, io1) = f (env, io)
        in (v, insert env1 n v, io1)))
    Bin op e1 e2 ->
      compile e1 (\f1 ->
        compile e2 (\f2 ->
          cont (\(env, io) ->
            let (v1, env1, io1) = f1 (env, io)
                (v2, env2, io2) = f2 (env1, io1)
            in ((bin op) v1 v2, env2, io2))))
    Do [] -> undefined
    Do [exp] -> compile exp (\f -> cont (\(env, io) -> f (env, io)))
    Do (exp:exps) -> compile (Do exps) (\rest ->
      compile exp (\f ->
        cont (\(env, io) -> let (_, env1, io1) = f (env, io)
                            in rest (env1, io1))))
    While condition body ->
      compile condition (\cond ->
        compile body (\bod ->
          cont (\(env, io) ->
            let loop = \(env, io) -> let (c, env1, io1) = cond (env, io)
                                   in if 1 == c
                                      then let (_, env2, io2) = bod (env1, io1)
                                           in loop (env2, io2)
                                      else (bottom, env1, io1)
            in loop (env, io))))
    Print exp -> compile exp (\f -> cont (\(env, io) -> let (v, env1, io1) = f (env, io)
                                                        in (v, env1, append io1 v)))

data StackOp
  = StackPush Value
  | StackSet Name
  | StackGet Name
  | StackBin Op
  | StackJump Int
  | StackJumpIfZero Int
  | StackPrint
  | StackEnd
  deriving (Show)

stack_compile :: Exp -> [StackOp]
stack_compile exp =
  loop 0 exp <> [StackEnd]
  where
  loop :: Int -> Exp -> [StackOp]
  loop count = \case
    Lit v -> [StackPush v]
    Var n -> [StackGet n]
    Set n e -> loop count e <> [StackSet n]
    Bin op e1 e2 ->
      let c1 = loop count e1
          c2 = loop (count + length c1) e2
      in c1 <> c2 <> [StackBin op]
    Do exps -> snd $ foldl (\(count, code) exp ->
                              let c = loop count exp
                              in (count + length c, code <> c))
                           (count, [])
                           exps
    While cond body ->
      let cc = loop count cond
          cb = loop (count + length cc + 1) body
      in cc <> [StackJumpIfZero (count + length cc + 1 + length cb + 1)]
            <> cb
            <> [StackJump count]
    Print exp -> loop count exp <> [StackPrint]

stack_exec :: [StackOp] -> Env -> TweIO
stack_exec code env =
  loop 0 [] env Halt
  where
  code' :: Data.Vector StackOp
  code' = Data.Vector.fromList code
  loop :: Int -> [Value] -> Env -> TweIO -> TweIO
  loop ip stack env io = case (Data.Vector.!) code' ip of
    StackPush v -> loop (ip + 1) (v:stack) env io
    StackSet n -> loop (ip + 1) (tail stack) (insert env n (head stack)) io
    StackGet n -> loop (ip + 1) (lookup env n : stack) env io
    StackBin op -> loop (ip + 1) ((bin op) (stack !! 1) (stack !! 0) : drop 2 stack) env io
    StackJump i -> loop i stack env io
    StackJumpIfZero i -> if (stack !! 0) == 0
                         then loop i (tail stack) env io
                         else loop (ip + 1) (tail stack) env io
    StackPrint -> loop (ip + 1) (tail stack) env (append io (head stack))
    StackEnd -> io

stack_exec_cont :: [StackOp] -> Env -> TweIO
stack_exec_cont code =
  \env -> (code' Data.Vector.! 0) 0 env []
  where
  code' :: Data.Vector (Int -> Env -> [Value] -> TweIO)
  code' = Data.Vector.fromList $ (flip map) code $ \case
    StackPush v -> \ip env stack -> (code' Data.Vector.! (ip+1)) (ip+1) env (v:stack)
    StackSet n -> \ip env stack -> (code' Data.Vector.! (ip+1)) (ip+1) (insert env n (head stack)) (tail stack)
    StackGet n -> \ip env stack -> (code' Data.Vector.! (ip+1)) (ip+1) env (lookup env n : stack)
    StackBin op -> \ip env stack -> (code' Data.Vector.! (ip+1)) (ip+1) env ((bin op) (stack !! 1) (stack !! 0) : drop 2 stack)
    StackJump i -> \_ env stack -> (code' Data.Vector.! i) i env stack
    StackJumpIfZero i -> \ip env stack ->
      let next = if (head stack) == 0 then i else ip+1
      in (code' Data.Vector.! next) next env (tail stack)
    StackPrint -> \ip env stack -> put ((code' Data.Vector.! (ip+1)) (ip+1) env (tail stack)) (head stack)
    StackEnd -> \_ _ _ -> Halt


data Switch
  = Perf
  | Vals
  | Test

switch :: Switch
switch = Perf

main :: IO ()
main = case switch of
  Test -> do
    let run e = stack_exec_cont (stack_compile e) (insert mt_env (Name "t") 0)
    print $ run sam
    print $ run $ fact 3
    print $ run neil
  Vals -> do
    let env = insert mt_env (Name "t") 0
    _ <- for [("tree_walk_eval", tree_walk_eval)
             ,("twe_cont", twe_cont)
             ,("twe_mon", twe_mon)
             ,("closure_eval", closure_eval)
             ,("closure_cont", closure_cont)
             ,("stack_exec", \e -> stack_exec (stack_compile e))
             ]
             (\(n, f) -> do
               putStrLn n
               print $ f sam env
               print $ f (fact 3) env
               print $ f neil env)
    pure()
  Perf -> do
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
    let ntimes :: (Env -> TweIO) -> Int -> IO ()
        ntimes f n =
          if n == 0
          then return ()
          else do
            void $ Control.Exception.evaluate $ Control.DeepSeq.rnf $ f (insert mt_env (Name "t") (Value n))
            ntimes f (n - 1)
    let bench s f = do
          ntimes f 3
          start <- now
          ntimes f 10
          end <- now
          printDur s start end
    bench "direct" direct
    bench "tree_walk_eval" (tree_walk_eval neil)
    bench "twe_cont" (twe_cont neil)
    bench "twe_mon" (twe_mon neil)
    let ce = closure_eval neil
    bench "closure_eval" ce
    let cc = closure_cont neil
    bench "closure_cont" cc
    let se = let ops = stack_compile neil in stack_exec ops
    bench "stack_exec" se
    let sec = let ops = stack_compile neil in stack_exec_cont ops
    bench "stack_exec_cont" sec
    pure ()
