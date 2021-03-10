module Main (main)
where

import Prelude hiding (exp)
--import Control.Monad (ap,liftM)
import Data.Map (Map)
import qualified Data.Map as Map

data Exp where
  Lit :: Int -> Exp
  Var :: String -> Exp
  Set :: String -> Exp -> Exp
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
  Do [
    Set "x" (Lit 100),
    Set "i" (Lit 1000),
    While (NotEq (Lit 0) (Var "i"))
      (Do [
        Set "x" (Add (Add (Add (Var "x") (Lit 4)) (Var "x")) (Lit 3)),
        Set "x" (Add (Add (Var "x") (Lit 2)) (Lit 4)),
        Set "i" (Add (Lit (-1)) (Var "i"))
      ]),
    Var "x"
    ]

fact :: Int -> Exp
fact x =
  Do [
    Set "acc" (Lit 1),
    Set "i" (Lit x),
    While (NotEq (Lit 0) (Var "i"))
      (Do [
        Set "acc" (Mul (Var "acc") (Var "i")),
        Set "i" (Sub (Var "i") (Lit 1)),
        Print (Var "acc"),
        Print (Var "i")
      ]),
    Print (Var "acc")
  ]

sam :: Exp
sam = Do [
  Set "x" (Lit 13),
  Print (Var "x"),
  Set "x" (Add (Var "x") (Var "x")),
  Print (Var "x")
  ]

treeWalkEval :: Exp -> (Int, [Int], Map String Int)
treeWalkEval ex =
  loop ex [] env0
  where
  env0 :: Map String Int
  env0 = Map.empty
  loop :: Exp -> [Int] -> Map String Int -> (Int, [Int], Map String Int)
  loop exp pr env =
    case exp of
      Lit v -> (v, pr, env)
      Var n -> (maybe undefined id (Map.lookup n env), pr, env)
      Set n expr -> let (v, p, e) = loop expr pr env
                    in (v, p, Map.insert n v e)
      Add e1 e2 -> do
        let (v1, p', e') = loop e1 pr env
        let (v2, p'', e'') = loop e2 p' e'
        ((v1 + v2), p'', e'')
      Sub e1 e2 -> do
        let (v1, p', e') = loop e1 pr env
        let (v2, p'', e'') = loop e2 p' e'
        ((v1 - v2), p'', e'')
      Mul e1 e2 -> do
        let (v1, p', e') = loop e1 pr env
        let (v2, p'', e'') = loop e2 p' e'
        ((v1 * v2), p'', e'')
      NotEq e1 e2 -> do
        let (v1, p', e') = loop e1 pr env
        let (v2, p'', e'') = loop e2 p' e'
        (if (v1 /= v2) then 1 else 0, p'', e'')
      Do (exps) -> foldl (\(_, p, e) expr -> loop expr p e) (undefined, pr, env) exps
      While condition body -> do
        let (c, p', e') = loop condition pr env
        if c == 1
        then do
          let (_, p'', e'') = loop body p' e'
          loop (While condition body) p'' e''
        else (undefined, p', e')
      Print expr -> let (v, p, e) = loop expr pr env
                    in (v, v:p, e)

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
  putStrLn (show neil)
  putStrLn (show $ fact 3)
  putStrLn (show sam)
  print $ treeWalkEval sam
  print $ treeWalkEval $ fact 3
  print $ treeWalkEval $ neil
  pure ()
