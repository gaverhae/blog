module Main (main)
where

import Prelude hiding (exp,lookup)
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
import qualified System.Clock

data Op
  = Add
  | NotEq
  deriving Show

data Exp
 = Lit Int
 | Var Int
 | Set Int Exp
 | Bin Op Exp Exp
 | Do Exp Exp
 | While Exp Exp
  deriving Show

bin :: Op -> Int -> Int -> Int
bin = \case
  Add -> (+)
  NotEq -> \v1 v2 -> if v1 /= v2 then 1 else 0

ast :: Exp
ast =
  -- x = 100
  (Do (Set 0 (Lit 100))
      -- i = 1000
      (Do (Set 1 (Lit 1000))
          -- for (; i != 0;)
          (Do (While (Bin NotEq (Lit 0)
                                (Var 1))
                     -- x = (((x + 4) + x) + 3)
                     (Do (Set 0 (Bin Add (Bin Add (Bin Add (Var 0)
                                                           (Lit 4))
                                                  (Var 0))
                                         (Lit 3)))
                         -- x = ((x + 2) + 4)
                         (Do (Set 0 (Bin Add (Bin Add (Var 0)
                                                      (Lit 2))
                                             (Lit 4)))
                             -- i = i + (-1)
                             (Set 1 (Bin Add (Lit (-1))
                                             (Var 1))))))
              -- return x
              (Var 0))))

direct :: () -> Int
direct _ =
  loop 100 1000
  where
  loop :: Int -> Int -> Int
  loop x0 i =
    if (0 == i)
    then x0
    else let x1 = x0 + 4 + x0 + 3
             x2 = x1 + 2 + 4
         in loop x2 (i - 1)

newtype Env = Env (Data.Map Int Int)
  deriving Show

bottom :: Int
bottom = undefined

lookup :: Env -> Int -> Int
lookup (Env m) n = maybe bottom id (Data.Map.lookup n m)

insert :: Env -> Int -> Int -> Env
insert (Env m) n v = Env $ Data.Map.insert n v m

mt_env :: Env
mt_env = Env Data.Map.empty

naive_ast_walk :: Exp -> Int
naive_ast_walk ex =
  let (r, _) = loop ex mt_env in r
  where
  loop :: Exp -> Env -> (Int, Env)
  loop exp0 env0 =
    case exp0 of
      Lit v -> (v, env0)
      Var n -> (lookup env0 n, env0)
      Set n exp1 -> let (v, env1) = loop exp1 env0
                    in (v, insert env1 n v)
      Bin op e1 e2 -> do
        let (v1, env1) = loop e1 env0
        let (v2, env2) = loop e2 env1
        ((bin op) v1 v2, env2)
      Do first rest -> do
        let (_, env1) = loop first env0
        loop rest env1
      While condition body -> do
        let (c, env1) = loop condition env0
        if c == 1
        then do
          let (_, env2) = loop body env1
          loop exp0 env2
        else (bottom, env1)

twe_cont :: Exp -> Env -> Int
twe_cont e env =
  loop e env (\_ r -> r)
  where
  loop :: Exp -> Env -> (Env -> Int -> Int) -> Int
  loop exp env cont =
    case exp of
      Lit v -> cont env v
      Var n -> cont env (lookup env n)
      Set n exp -> loop exp env (\env v -> cont (insert env n v) v)
      Bin op e1 e2 -> loop e1 env (\env v1 -> loop e2 env (\env v2 -> cont env ((bin op) v1 v2)))
      Do first rest -> loop first env (\env _ -> loop rest env cont)
      While condition body -> loop condition env (\env condition_value ->
        if (1 == condition_value)
        then loop body env (\env _ ->
          loop (While condition body) env (\env v -> cont env v))
        else cont env bottom)

data EvalExec a where
  EvalBind :: EvalExec a -> (a -> EvalExec b) -> EvalExec b
  EvalReturn :: a -> EvalExec a
  EvalLookup :: Int -> EvalExec Int
  EvalSet :: Int -> Int -> EvalExec ()

instance Functor EvalExec where fmap = liftM
instance Applicative EvalExec where pure = return; (<*>) = ap
instance Monad EvalExec where return = EvalReturn; (>>=) = EvalBind

twe_mon :: Exp -> Int
twe_mon exp =
  exec (eval exp) mt_env (\_ r -> r)
  where
  eval :: Exp -> EvalExec Int
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
    Do first rest -> do
      _ <- eval first
      eval rest
    While condition body -> do
      c <- eval condition
      if 1 == c
      then do
        _ <- eval body
        eval (While condition body)
      else return bottom

  exec :: EvalExec a -> Env -> (Env -> a -> Int) -> Int
  exec m env cont = case m of
    EvalBind prev step -> exec prev env (\env ret -> exec (step ret) env cont)
    EvalReturn v -> cont env v
    EvalLookup n -> cont env (lookup env n)
    EvalSet n v -> cont (insert env n v) ()

closure_eval :: Exp -> Env -> Int
closure_eval e =
  let c = compile e in \env -> (fst $ c env)
  where
  compile :: Exp -> Env -> (Int, Env)
  compile = \case
    Lit v -> \env -> (v, env)
    Var n -> \env -> (lookup env n, env)
    Set n exp ->
      let f = compile exp
      in \env ->
        let (v1, env1) = f env
        in (v1, insert env1 n v1)
    Bin op e1 e2 ->
      let f1 = compile e1
          f2 = compile e2
      in \env ->
        let (v1, env1) = f1 env
            (v2, env2) = f2 env1
        in ((bin op) v1 v2, env2)
    Do first rest ->
      let f = compile first
          r = compile rest
      in \env ->
        let (_, env1) = f env
        in r env1
    While condition body ->
      let cond = compile condition
          bod = compile body
          loop = \env ->
            let (c, env1) = cond env
            in if 1 == c
               then let (_, env2) = bod env1
                    in loop (env2)
               else (bottom, env1)
      in loop

closure_cont :: Exp -> Env -> Int
closure_cont e =
  let f = compile e (\f env -> f env)
  in \env -> snd $ f env
  where
  compile :: Exp -> ((Env -> (Env, Int)) -> Env -> (Env, Int)) -> Env -> (Env, Int)
  compile exp cont = case exp of
    Lit v -> cont (\env -> (env, v))
    Var n -> cont (\env -> (env, lookup env n))
    Set n exp -> compile exp (\f ->
      cont (\env ->
        let (env1, v) = f env
        in (insert env1 n v, v)))
    Bin op e1 e2 ->
      compile e1 (\f1 ->
        compile e2 (\f2 ->
          cont (\env ->
            let (env1, v1) = f1 env
                (env2, v2) = f2 env1
            in (env2, (bin op) v1 v2))))
    Do first rest -> compile first (\f ->
      compile rest (\r -> cont (\env -> r (fst $ f env))))
    While condition body ->
      compile condition (\cond ->
        compile body (\bod ->
          cont (\env ->
            let loop = \env -> let (env1, c) = cond env
                                   in if 1 == c
                                      then let (env2, _) = bod env1
                                           in loop env2
                                      else (env1, bottom)
            in loop env)))

data StackOp
  = StackPush Int
  | StackSet Int
  | StackGet Int
  | StackBin Op
  | StackJump Int
  | StackJumpIfZero Int
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
    Do first rest ->
      let c1 = loop count first
          c2 = loop (count + length c1) rest
      in c1 <> c2
    While cond body ->
      let cc = loop count cond
          cb = loop (count + length cc + 1) body
      in cc <> [StackJumpIfZero (count + length cc + 1 + length cb + 1)]
            <> cb
            <> [StackJump count]

stack_exec :: [StackOp] -> Env -> Int
stack_exec code env =
  loop 0 [] env
  where
  code' :: Data.Vector StackOp
  code' = Data.Vector.fromList code
  loop :: Int -> [Int] -> Env -> Int
  loop ip stack env = case (Data.Vector.!) code' ip of
    StackPush v -> loop (ip + 1) (v:stack) env
    StackSet n -> loop (ip + 1) (tail stack) (insert env n (head stack))
    StackGet n -> loop (ip + 1) (lookup env n : stack) env
    StackBin op -> loop (ip + 1) ((bin op) (stack !! 1) (stack !! 0) : drop 2 stack) env
    StackJump i -> loop i stack env
    StackJumpIfZero i -> if (stack !! 0) == 0
                         then loop i (tail stack) env
                         else loop (ip + 1) (tail stack) env
    StackEnd -> head stack

stack_exec_cont :: [StackOp] -> Env -> Int
stack_exec_cont code =
  \env -> (code' Data.Vector.! 0) 0 env []
  where
  code' :: Data.Vector (Int -> Env -> [Int] -> Int)
  code' = Data.Vector.fromList $ (flip map) code $ \case
    StackPush v -> \ip env stack -> (code' Data.Vector.! (ip+1)) (ip+1) env (v:stack)
    StackSet n -> \ip env stack -> (code' Data.Vector.! (ip+1)) (ip+1) (insert env n (head stack)) (tail stack)
    StackGet n -> \ip env stack -> (code' Data.Vector.! (ip+1)) (ip+1) env (lookup env n : stack)
    StackBin op -> \ip env stack -> (code' Data.Vector.! (ip+1)) (ip+1) env ((bin op) (stack !! 1) (stack !! 0) : drop 2 stack)
    StackJump i -> \_ env stack -> (code' Data.Vector.! i) i env stack
    StackJumpIfZero i -> \ip env stack ->
      let next = if (head stack) == 0 then i else ip+1
      in (code' Data.Vector.! next) next env (tail stack)
    StackEnd -> \r _ _ -> r


data Switch
  = Perf
  | Vals
  | Test

switch :: Switch
switch = Perf

main :: IO ()
main = case switch of
  Test -> do
    let run e = stack_exec_cont (stack_compile e) (insert mt_env 2 0)
    print $ run ast
  Vals -> do
    _ <- for [("naive_ast_walk", naive_ast_walk)
             ,("twe_cont", \ast -> twe_cont ast mt_env)
             ,("twe_mon", twe_mon)
             ,("closure_eval", \ast -> closure_eval ast mt_env)
             ,("closure_cont", \ast -> closure_cont ast mt_env)
             ,("stack_exec", \ast -> stack_exec (stack_compile ast) mt_env)
             ]
             (\(n, f) -> do
               putStrLn n
               print $ f ast)
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
    let ntimes :: (() -> Int) -> Int -> IO ()
        ntimes f n =
          if n == 0
          then return ()
          else do
            void $ Control.Exception.evaluate $ Control.DeepSeq.rnf $ f ()
            ntimes f (n - 1)
    let bench s f = do
          start <- now
          ntimes f 3
          end <- now
          printDur s start end
          start <- now
          ntimes f 10
          end <- now
          printDur s start end
    bench "direct" direct
    bench "naive_ast_walk" (\_ -> naive_ast_walk ast)
    bench "twe_cont" (\_ -> twe_cont ast mt_env)
    bench "twe_mon" (\_ -> twe_mon ast)
    let ce = closure_eval ast
    bench "closure_eval" (\_ -> ce mt_env)
    let cc = closure_cont ast
    bench "closure_cont" (\_ -> cc mt_env)
    let se = let ops = stack_compile ast in stack_exec ops
    bench "stack_exec" (\_ -> se mt_env)
    let sec = let ops = stack_compile ast in stack_exec_cont ops
    bench "stack_exec_cont" (\_ -> sec mt_env)
    pure ()
