module Main where

data E = Var String
       | Abs String E
       | App E E
       | Add E E
       | Num Int
       deriving (Eq, Show)

eval :: E -> (String -> E) -> E
eval exp env = case exp of
  Var s -> env s
  Abs s e -> exp
  App abs arg -> case abs of
    Abs n e -> eval e (\s -> if s == n then arg else env s)
    _ -> error $ "Trying to apply non-function value: " ++ show abs
  Add a b -> case (eval a env, b) of
    (Num x, Num y) -> Num (x + y)
    _ -> error $ "Trying to add non-numbers: (" ++ show a ++ ", " ++ show b ++ ")"
  Num i -> Num i

initenv var = error $ "Could not find " ++ var

assert x = if x then return () else error "bad"

assertExp e1 e2 = assert $ eval e1 initenv == e2

main :: IO ()
main = do
  assert $ 1 == 1
  -- \x.x => \x.x
  assertExp (Abs "x" (Var "x"))
            (Abs "x" (Var "x"))
  -- (\x.x) 2 => 2
  assertExp (App (Abs "x" (Var "x")) (Num 2))
            (Num 2)
  -- (\x.x + 1) 10 => 11
  assertExp (App (Abs "x" (Add (Var "x") (Num 1))) (Num 10))
            (Num 11)
  putStrLn "All good"
