module Main (main)
where

import Prelude hiding (exp)
import Data.Map (Map)
import qualified Data.Map as Map

newtype Value = Value Int
  deriving Show

newtype Name = Name String
  deriving (Eq, Ord, Show)

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
  Do [
    Set (Name "x") (Lit (Value 100)),
    Set (Name "i") (Lit (Value 1000)),
    While (NotEq (Lit (Value 0)) (Var (Name "i")))
      (Do [
        Set (Name "x") (Add (Add (Add (Var (Name "x")) (Lit (Value 4))) (Var (Name "x"))) (Lit (Value 3))),
        Set (Name "x") (Add (Add (Var (Name "x")) (Lit (Value 2))) (Lit (Value 4))),
        Set (Name "i") (Add (Lit (Value (-1))) (Var (Name "i")))
      ]),
    Var (Name "x")
    ]

fact :: Int -> Exp
fact x =
  Do [
    Set (Name "acc") (Lit (Value 1)),
    Set (Name "i") (Lit (Value x)),
    While (NotEq (Lit (Value 0)) (Var (Name "i")))
      (Do [
        Set (Name "x") (Mul (Var (Name "acc")) (Var (Name "i"))),
        Set (Name "i") (Sub (Var (Name "i")) (Lit (Value 1))),
        Print (Var (Name "acc"))
      ]),
    Print (Var (Name "acc"))
  ]

sam :: Exp
sam = Do [
  Set (Name "x") (Lit (Value 13)),
  Print (Var (Name "x")),
  Set (Name "x") (Add (Var (Name "x")) (Var (Name "x"))),
  Print (Var (Name "x"))
  ]

newtype Env = Env (Map Name Value)
  deriving Show

newtype Printed = Printed [Int]
  deriving Show

treeWalkEval :: Exp -> (Value, Printed, Env)
treeWalkEval ex =
  loop ex (Printed []) env0
  where
  env0 :: Env
  env0 = Env Map.empty
  loop :: Exp -> Printed -> Env -> (Value, Printed, Env)
  loop exp (Printed pr) (Env env) =
    case exp of
      Lit v -> (v, (Printed pr), (Env env))
      Var n -> (maybe undefined id (Map.lookup n env), (Printed pr), (Env env))
      Set n expr -> let (v, p, (Env e)) = loop expr (Printed pr) (Env env)
                    in (v, p, (Env $ Map.insert n v e))
      Add e1 e2 -> do
        let (Value v1, p', e') = loop e1 (Printed pr) (Env env)
        let (Value v2, p'', e'') = loop e2 p' e'
        (Value (v1 + v2), p'', e'')
      Sub {} -> undefined
      Mul {} -> undefined
      NotEq {} -> undefined
      Do (exps) -> foldl (\(_, p, e) expr -> loop expr p e) (undefined, (Printed pr), (Env env)) exps
      While {} -> undefined
      Print expr -> let (v, (Printed p), e) = loop expr (Printed pr) (Env env)
                    in (v, (Printed $ concat [p, pr]), e)

main :: IO ()
main = do
  putStrLn (show neil)
  putStrLn (show $ fact 3)
  putStrLn (show sam)
  print $ treeWalkEval sam
  pure ()
