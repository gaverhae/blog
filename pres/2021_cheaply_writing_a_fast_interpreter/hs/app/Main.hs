module Main (main)
where

import Prelude hiding (exp,lookup)
import qualified Control.DeepSeq
import Control.Monad (ap,liftM,void,forM)
import qualified Control.Monad.ST
import qualified Data.Map as Data (Map)
import qualified Data.Map
import qualified Data.Text.Lazy.Builder
import qualified Data.Vector as Data (Vector)
import qualified Data.Vector
import qualified Data.Vector.Unboxed.Mutable
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

direct :: Int -> Int
direct n =
  loop 100 (n + 1000)
  where
  loop :: Int -> Int -> Int
  loop x0 i =
    if (n == i)
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

naive_ast_walk :: Exp -> Int -> Int
naive_ast_walk ex =
  \n -> let (r, _) = loop ex (insert mt_env 13 n) in r
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

twe_cont :: Exp -> Int -> Int
twe_cont e =
  \n -> loop e (insert mt_env 13 n) (\_ r -> r)
  where
  loop :: Exp -> Env -> (Env -> Int -> Int) -> Int
  loop exp env cont =
    case exp of
      Lit v -> cont env v
      Var n -> cont env (lookup env n)
      Set n exp -> loop exp env (\env v -> cont (insert env n v) v)
      Bin op e1 e2 -> loop e1 env (\env v1 ->
        loop e2 env (\env v2 ->
          cont env ((bin op) v1 v2)))
      Do first rest -> loop first env (\env _ -> loop rest env cont)
      While condition body -> loop condition env (\env condition_value ->
        if (1 == condition_value)
        then loop body env (\env _ -> loop exp env cont)
        else cont env bottom)

data EvalExec a where
  EvalBind :: EvalExec a -> (a -> EvalExec b) -> EvalExec b
  EvalReturn :: a -> EvalExec a
  EvalLookup :: Int -> EvalExec Int
  EvalSet :: Int -> Int -> EvalExec ()

instance Functor EvalExec where fmap = liftM
instance Applicative EvalExec where pure = return; (<*>) = ap
instance Monad EvalExec where return = EvalReturn; (>>=) = EvalBind

twe_mon :: Exp -> Int -> Int
twe_mon exp =
  \n -> exec (eval exp) (insert mt_env 13 n) (\_ r -> r)
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

compile_to_closure :: Exp -> Int -> Int
compile_to_closure e =
  let !c = compile e in \n -> (fst $ c (insert mt_env 13 n))
  where
  compile :: Exp -> Env -> (Int, Env)
  compile = \case
    Lit v -> \env -> (v, env)
    Var n -> \env -> (lookup env n, env)
    Set n exp ->
      let !f = compile exp
      in \env ->
        let (v1, env1) = f env
        in (v1, insert env1 n v1)
    Bin op e1 e2 ->
      let !f1 = compile e1
          !f2 = compile e2
      in \env ->
        let (v1, env1) = f1 env
            (v2, env2) = f2 env1
        in ((bin op) v1 v2, env2)
    Do first rest ->
      let !f = compile first
          !r = compile rest
      in \env ->
        let (_, env1) = f env
        in r env1
    While condition body ->
      let !cond = compile condition
          !bod = compile body
          !loop = \env ->
            let (c, env1) = cond env
            in if 1 == c
               then let (_, env2) = bod env1
                    in loop (env2)
               else (bottom, env1)
      in loop

data StackOp
  = StackPush Int
  | StackSet Int
  | StackGet Int
  | StackBin Op
  | StackJump Int
  | StackJumpIfZero Int
  | StackEnd
  deriving (Show)

compile_stack :: Exp -> [StackOp]
compile_stack exp =
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

exec_stack :: [StackOp] -> Int -> Int
exec_stack code =
  \n -> loop 0 [n]
  where
  loop :: Int -> [Int] -> Int
  loop ip stack = case code !! ip of
    StackPush v -> loop (ip + 1) (push stack v)
    StackSet n -> let (v, s) = pop stack
                  in loop (ip + 1) (set s n v)
    StackGet n -> loop (ip + 1) (push stack (get stack n))
    StackBin op -> let (a1, s1) = pop stack
                       (a2, s2) = pop s1
                   in loop (ip + 1) (push s2 ((bin op) a2 a1))
    StackJump i -> loop i stack
    StackJumpIfZero i -> let (v, s) = pop stack
                         in if v == 0
                            then loop i s
                            else loop (ip + 1) s
    StackEnd -> fst (pop stack)
  pop :: [a] -> (a, [a])
  pop ls = (head ls, tail ls)
  push :: [a] -> a -> [a]
  push ls a = a : ls
  get :: [a] -> Int -> a
  get ls pos = (reverse ls) !! pos
  set :: [a] -> Int -> a -> [a]
  set ls pos val =
    let r = reverse ls
    in reverse (take pos r ++ val : drop (pos+1) r)

exec_stack_2 :: [StackOp] -> Int -> Int
exec_stack_2 ls_code =
  \n -> Control.Monad.ST.runST $ do
    init_stack <- Data.Vector.Unboxed.Mutable.unsafeNew (256 + n)
    go init_stack
  where
  code :: Data.Vector StackOp
  !code = Data.Vector.fromList ls_code
  go :: forall s. Data.Vector.Unboxed.Mutable.MVector s Int -> Control.Monad.ST.ST s Int
  go stack = do
    loop 0 0
    where
    loop :: Int -> Int -> Control.Monad.ST.ST s Int
    loop ip top = case (Data.Vector.!) code ip of
      StackPush v -> do
        top <- push top v
        loop (ip + 1) top
      StackSet n -> do
        (v, top) <- pop top
        set n v
        loop (ip + 1) (max top (n + 1))
      StackGet n -> do
        v <- get n
        top <- push top v
        loop (ip + 1) top
      StackBin op -> do
        (a1, top) <- pop top
        (a2, top) <- pop top
        top <- push top ((bin op) a2 a1)
        loop (ip + 1) top
      StackJump i -> loop i top
      StackJumpIfZero i -> do
        (v, top) <- pop top
        if v == 0
        then loop i top
        else loop (ip + 1) top
      StackEnd -> do
        (v, _) <- pop top
        return v
    pop :: Int -> Control.Monad.ST.ST s (Int, Int)
    pop top = do
      v <- Data.Vector.Unboxed.Mutable.read stack (top - 1)
      return (v, top - 1)
    push :: Int -> Int -> Control.Monad.ST.ST s Int
    push top v = do
      Data.Vector.Unboxed.Mutable.write stack top v
      return (top + 1)
    get :: Int -> Control.Monad.ST.ST s Int
    get top = do
      v <- Data.Vector.Unboxed.Mutable.read stack top
      return v
    set :: Int -> Int -> Control.Monad.ST.ST s ()
    set pos val = Data.Vector.Unboxed.Mutable.write stack pos val

bench :: Control.DeepSeq.NFData a => [Int] -> (String, Int -> a) -> IO ()
bench ns (name, f) = do
  let now = System.Clock.getTime System.Clock.Monotonic
  let raw_string = Formatting.now . Data.Text.Lazy.Builder.fromString
  let printDur = Formatting.fprint
                   (Formatting.Formatters.string
                    Formatting.%
                    raw_string " ("
                    Formatting.%
                    Formatting.Formatters.shown
                    Formatting.%
                    raw_string " runs): "
                    Formatting.%
                    Formatting.Clock.timeSpecs
                    Formatting.%
                    raw_string " ("
                    Formatting.%
                    Formatting.Formatters.string
                    Formatting.%
                    raw_string ")\n")
  let ntimes :: Int -> IO ()
      ntimes 0 = return ()
      ntimes n = Control.DeepSeq.deepseq (f n) (ntimes (n - 1))
  let per_run t1 t2 n = do
        let i1 = System.Clock.toNanoSecs t1
            i2 = System.Clock.toNanoSecs t2
            dur = ((i2 - i1) `div` n)
            (d, unit) = if dur > 10000000
                        then (dur `div` 1000000, "ms/run")
                        else if dur > 10000
                        then (dur `div` 1000, "µs/run")
                        else (dur, "ns/run")
        show d <> " " <> unit
  let run :: Int -> IO ()
      run n = do
        start <- now
        ntimes n
        end <- now
        printDur name n start end (per_run start end (fromIntegral n))
  void $ forM ns (\n -> run n)
  return ()

functions :: [(String, Int -> Int)]
functions = [
  ("direct", direct),
  ("naive_ast_walk", naive_ast_walk ast),
  ("twe_mon", twe_mon ast),
  ("compile_to_closure", compile_to_closure ast),
  ("twe_cont", twe_cont ast),
  ("exec_stack", exec_stack (compile_stack ast)),
  ("exec_stack_2", exec_stack_2 (compile_stack ast))
  ]

main :: IO ()
main = do
  void $ forM functions (bench [30, 3000])

_test :: IO ()
_test = do
  print $ (map (\(_, f) -> f 0) functions)
  void $ forM functions (bench [3, 30])
  pure ()
