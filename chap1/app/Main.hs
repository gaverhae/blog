module Main where

data E = Var String
       | Abs String E
       | App E E
       | Add E E
       | Num Int
       deriving (Eq, Show)

eval :: E -> [(String, E)] -> E
eval exp env = case exp of
  Var s -> find env s
  Abs h e -> Abs h $ eval e $ (h, Var h):env
  App abs arg -> case result of
    Abs h e -> eval e $ (h, arg):env
    Var s -> App (Var s) (eval arg env)
    _ -> App result (eval arg env)
    where result = eval abs env
  Add a b -> case (a', b') of
    (Num x, Num y) -> Num (x + y)
    _ -> Add a' b'
    where (a', b') = (eval a env, eval b env)
  Num i -> Num i

find env s =
  case env of
       [] -> Var s
       (n, e):tl -> if n == s then e else find tl s

initenv = []

test env e1 e2 =
  if result == e2
     then return ()
     else error $ "Error evaluating: \n(" ++ show e1 ++ ")\nto:\n(" ++ show result ++ ")\nwhile expecting:\n(" ++ show e2 ++ ")"
  where result = eval e1 env

main :: IO ()
main = do
  -- \x.x => \x.x
  test initenv
       (Abs "x" (Var "x"))
       (Abs "x" (Var "x"))
  -- (\x.x) 2 => 2
  test initenv
       (App (Abs "x" (Var "x")) (Num 2))
       (Num 2)
  -- (\x.x + 1) 10 => 11
  test initenv
       (App (Abs "x" (Add (Var "x") (Num 1))) (Num 10))
       (Num 11)
  -- (\x.x) (\y.y) => (\y.y)
  test initenv
       (App (Abs "x" (Var "x")) (Abs "y" (Var "y")))
       (Abs "y" (Var "y"))
  -- (\x.x) (\y.y) z => z
  test initenv
       (App (App (Abs "x" (Var "x"))
                 (Abs "y" (Var "y")))
            (Var "z"))
       (Var "z")
  -- (\x.xy) z => xz
  test initenv
       (App (Abs "x" (App (Var "x") (Var "y")))
            (Var "z"))
       (App (Var "z") (Var "y"))
  -- (\xy.xy) (\z.a) => (\y.a)
  test initenv
       (App (Abs "x" (Abs "y" (App (Var "x") (Var "y"))))
            (Abs "z" (Var "a")))
       (Abs "y" (Var "a"))
  test initenv
       (App (App (Abs "x" (Abs "y" (App (Var "x") (Var "y"))))
                 (Abs "z" (Var "a")))
            (Num 1))
       (Var "a")
  -- faking names with extra nesting
  -- inc = \x.(x + 1)
  -- inc 10
  test initenv
       (App (Abs "inc" (App (Var "inc") (Num 10)))
            (Abs "x" (Add (Var "x") (Num 1))))
       (Num 11)
  -- cheating: adding names to env
  test [("inc", Abs "x" (Add (Var "x") (Num 1)))]
       (App (Var "inc") (Num 10))
       (Num 11)
  -- thrice f x = f (f (f x))
  -- thrice inc
  test [("inc", Abs "x" (Add (Var "x") (Num 1))),
        ("thrice", Abs "f" (Abs "x" (App (Var "f")
                                         (App (Var "f")
                                              (App (Var "f")
                                                   (Var "x"))))))]
       (App (Var "thrice") (Var "inc"))
       (Abs "x" (App (Var "inc")
                     (App (Var "inc")
                          (App (Var "inc")
                               (Var "x")))))
  -- (\x. inc x) 100
  test [("inc", Abs "x" (Add (Var "x") (Num 1)))]
       (App (Abs "x" (App (Var "inc")
                          (Var "x")))
            (Num 100))
       (Num 101)
  --
--  test [("inc", Abs "x" (Add (Var "x") (Num 1)))]
--       (App (Abs "x" (App (Var "inc")
--                          (App (Var "inc")
--                               (App (Var "inc")
--                                    (Var "x")))))
--            (Num 100))
--       (Num 103)
  --
--  test [("inc", Abs "x" (Add (Var "x") (Num 1)))]
--       (App (Abs "x" (App (Var "inc")
--                          (App (Var "inc")
--                               (App (Var "inc")
--                                    (Var "x")))))
--            (Num 100))
--       (Num 103)
  -- thrice inc 100
--  test [("inc", Abs "x" (Add (Var "x") (Num 1))),
--        ("thrice", Abs "f" (Abs "x" (App (Var "f")
--                                         (App (Var "f")
--                                              (App (Var "f")
--                                                   (Var "x"))))))]
--       (App (App (Var "thrice") (Var "inc"))
--            (Num 100))
--       (Num 103)
  putStrLn "All good"
