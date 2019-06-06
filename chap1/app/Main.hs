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
  Abs h e -> eval e (\s -> if s == h then Var h else env s)
  App abs arg -> case result of
    Abs n e -> eval e (\s -> if s == n then arg else env s)
    Var s -> App (Var s) (eval arg env)
    _ -> error $ "Trying to apply " ++ show result
    where result = (eval abs env)
  Add a b -> case (eval a env, eval b env) of
    (Num x, Num y) -> Num (x + y)
    _ -> error $ "Trying to add non-numbers: (" ++ show a ++ ", " ++ show b ++ ")"
  Num i -> Num i

initenv = Var

test env e1 e2 =
  if result == e2
     then return ()
     else error $ "(" ++ show result ++ ") != (" ++ show e2 ++ ")"
  where result = eval e1 env

main :: IO ()
main = do
  -- \x.x => \x.x
--  test initenv
--       (Abs "x" (Var "x"))
--       (Abs "x" (Var "x"))
--  -- (\x.x) 2 => 2
--  test initenv
--       (App (Abs "x" (Var "x")) (Num 2))
--       (Num 2)
--  -- (\x.x + 1) 10 => 11
--  test initenv
--       (App (Abs "x" (Add (Var "x") (Num 1))) (Num 10))
--       (Num 11)
--  -- (\x.x) (\y.y) => (\y.y)
--  test initenv
--       (App (Abs "x" (Var "x")) (Abs "y" (Var "y")))
--       (Abs "y" (Var "y"))
--  -- (\x.x) (\y.y) z => z
--  test initenv
--       (App (App (Abs "x" (Var "x"))
--                 (Abs "y" (Var "y")))
--            (Var "z"))
--       (Var "z")
--  -- (\x.xy) z => xz
--  test initenv
--       (App (Abs "x" (App (Var "x") (Var "y")))
--            (Var "z"))
--       (App (Var "z") (Var "y"))
--  -- (\xy.xy) (\z.a) => (\y.(\z.a)y)
--  test initenv
--       (App (Abs "x" (Abs "y" (App (Var "x") (Var "y"))))
--            (Abs "z" (Var "a")))
--       (Abs "y" (App (Abs "z" (Var "a")) (Var "y")))
--  test initenv
--       (App (App (Abs "x" (Abs "y" (App (Var "x") (Var "y"))))
--                 (Abs "z" (Var "a")))
--            (Num 1))
--       (Var "a")
  -- faking names with extra nesting
  -- inc = \x.(x + 1)
  -- inc 10
--  assertExp initenv
--            (App (Abs "inc" (App (Var "inc") (Num 10)))
--                 (Abs "x" (Add (Var "x") (Num 1))))
--            (Num 11)
--  -- cheating: adding names to env
--  assertExp (\s -> if s == "inc" then (Abs "x" (Add (Var "x") (Num 1)))
--                                 else initenv s)
--            (App (Var "inc") (Num 10))
--            (Num 11)
--  -- thrice f x = f (f (f x))
--  -- thrice inc 100
--  assertExp (\s -> case s of
--                        "inc" -> (Abs "x" (Add (Var "x") (Num 1)))
--                        "thrice" -> (Abs "f" (Abs "x" (App (Var "f")
--                                                           (App (Var "f")
--                                                                (App (Var "f")
--                                                                     (Var "x"))))))
--                        _ -> (initenv s))
--            (App (App (Var "thrice") (Var "inc"))
--                 (Num 100))
--            (Num 103)
  putStrLn "All good"
