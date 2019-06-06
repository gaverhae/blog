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
  App abs arg -> case eval abs env of
    Abs n e -> eval e (\s -> if s == n then arg else env s)
    _ -> error $ "Trying to apply non-function value: " ++ show abs
  Add a b -> case (eval a env, eval b env) of
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
  -- (\x.x) (\y.y) => (\y.y)
  assertExp (App (Abs "x" (Var "x")) (Abs "y" (Var "y")))
            (Abs "y" (Var "y"))
  -- faking names with extra nesting
  -- inc = \x.(x + 1)
  -- inc 10
  assertExp (App (Abs "inc" (App (Var "inc") (Num 10)))
                 (Abs "x" (Add (Var "x") (Num 1))))
            (Num 11)
  -- thrice f x = f (f (f x))
  -- thrice inc 100
--  assertExp (App (Abs "thrice" (App (Abs "inc" (App (App (Var "thrice")
--                                                         (Var "inc"))
--                                                    (Num 100)))
--                                    (Abs "x" (Add (Var "x") (Num 1)))))
--                 (Abs "f" (Abs "x" (App (Var "f")
--                                        (App (Var "f")
--                                             (App (Var "f")
--                                                  (Var "x")))))))
--            (Num 103)
  putStrLn "All good"
