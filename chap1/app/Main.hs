module Main where

import qualified Data.Set as Set
import qualified Control.Monad

data E = Var String
       | Abs String E
       | App E E
       | Add E E
       | Num Int
       deriving (Eq, Show)

data Program = Program E Env
newtype Env = Env [(String, E)]


usedNames :: Program -> Set.Set String
usedNames (Program exp env) =
  used exp `Set.union` predefined env
  where
  used exp = case exp of
    Var s -> Set.singleton s
    Abs h e -> Set.insert h $ used e
    App abs arg -> used abs `Set.union` used arg
    Add a b -> used a `Set.union` used b
    Num i -> Set.empty
  predefined (Env e) = Set.fromList (map fst e) `Set.union` Set.unions (map (used . snd) e)

eval :: E -> [(String, E)] -> E
eval exp env = case exp of
  Var s -> find env s
  Abs h e -> Abs h $ eval e $ (h, Var h):env
  App abs arg -> case (eval abs env, eval arg env) of
    (Abs h e, arg') -> eval e $ (h, arg'):env
    (Var s, arg') -> App (Var s) arg'
    (a, b) -> App a b
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

test env e1 e2 s = do
  Control.Monad.when (result /= e2) $ error $ "Error evaluating: \n(" ++ show e1 ++ ")\nto:\n(" ++ show result ++ ")\nwhile expecting:\n(" ++ show e2 ++ ")"
  Control.Monad.when (Set.fromList s /= usedNames (Program e1 (Env env))) $ error $ "NameError in:\n" ++ show e1 ++  "\nfound\n" ++ show (usedNames (Program e1 (Env env))) ++ "\nexpecting\n" ++ show s
  where result = eval e1 env

main :: IO ()
main = do
  -- \x.x => \x.x
  test initenv
       (Abs "x" (Var "x"))
       (Abs "x" (Var "x"))
       ["x"]
  -- (\x.x) 2 => 2
  test initenv
       (App (Abs "x" (Var "x")) (Num 2))
       (Num 2)
       ["x"]
  -- (\x.x + 1) 10 => 11
  test initenv
       (App (Abs "x" (Add (Var "x") (Num 1))) (Num 10))
       (Num 11)
       ["x"]
  -- (\x.x) (\y.y) => (\y.y)
  test initenv
       (App (Abs "x" (Var "x")) (Abs "y" (Var "y")))
       (Abs "y" (Var "y"))
       ["x", "y"]
  -- (\x.x) (\y.y) z => z
  test initenv
       (App (App (Abs "x" (Var "x"))
                 (Abs "y" (Var "y")))
            (Var "z"))
       (Var "z")
       ["x", "y", "z"]
  -- (\x.xy) z => xz
  test initenv
       (App (Abs "x" (App (Var "x") (Var "y")))
            (Var "z"))
       (App (Var "z") (Var "y"))
       ["x", "y", "z"]
  -- (\xy.xy) (\z.a) => (\y.a)
  test initenv
       (App (Abs "x" (Abs "y" (App (Var "x") (Var "y"))))
            (Abs "z" (Var "a")))
       (Abs "y" (Var "a"))
       ["x", "y", "z", "a"]
  test initenv
       (App (App (Abs "x" (Abs "y" (App (Var "x") (Var "y"))))
                 (Abs "z" (Var "a")))
            (Num 1))
       (Var "a")
       ["x", "y", "z", "a"]
  -- faking names with extra nesting
  -- inc = \x.(x + 1)
  -- inc 10
  test initenv
       (App (Abs "inc" (App (Var "inc") (Num 10)))
            (Abs "x" (Add (Var "x") (Num 1))))
       (Num 11)
       ["inc", "x"]
  -- cheating: adding names to env
  test [("inc", Abs "x" (Add (Var "x") (Num 1)))]
       (App (Var "inc") (Num 10))
       (Num 11)
       ["inc", "x"]
  -- thrice f x = f (f (f x))
  -- thrice inc
  test [("inc", Abs "x" (Add (Var "x") (Num 1))),
        ("thrice", Abs "f" (Abs "x" (App (Var "f")
                                         (App (Var "f")
                                              (App (Var "f")
                                                   (Var "x"))))))]
       (App (Var "thrice") (Var "inc"))
       (Abs "x" (Add (Add (Add (Var "x") (Num 1)) (Num 1)) (Num 1)))
       ["inc", "x", "thrice", "f"]
  -- (\x. inc x) 100 => 101
  test [("inc", Abs "x" (Add (Var "x") (Num 1)))]
       (App (Abs "x" (App (Var "inc")
                          (Var "x")))
            (Num 100))
       (Num 101)
       ["inc", "x"]
  -- (\x. inc (inc x)) 100 => 102
  test [("inc", Abs "x" (Add (Var "x") (Num 1)))]
       (App (Abs "x" (App (Var "inc")
                          (App (Var "inc")
                               (Var "x"))))
            (Num 100))
       (Num 102)
       ["inc", "x"]
  -- (\x. inc (inc (inc x))) 100 => 103
  test [("inc", Abs "x" (Add (Var "x") (Num 1)))]
       (App (Abs "x" (App (Var "inc")
                          (App (Var "inc")
                               (App (Var "inc")
                                    (Var "x")))))
            (Num 100))
       (Num 103)
       ["inc", "x"]
  -- thrice inc 100
  test [("inc", Abs "x" (Add (Var "x") (Num 1))),
        ("thrice", Abs "f" (Abs "x" (App (Var "f")
                                         (App (Var "f")
                                              (App (Var "f")
                                                   (Var "x"))))))]
       (App (App (Var "thrice") (Var "inc"))
            (Num 100))
       (Num 103)
       ["inc", "x", "thrice", "f"]
  -- thrice (thrice inc) 100
  test [("inc", Abs "x" (Add (Var "x") (Num 1))),
        ("thrice", Abs "f" (Abs "x" (App (Var "f")
                                         (App (Var "f")
                                              (App (Var "f")
                                                   (Var "x"))))))]
       (App (App (Var "thrice")
                 (App (Var "thrice")
                      (Var "inc")))
            (Num 100))
       (Num 109)
       ["inc", "x", "thrice", "f"]
  -- twice twice
  test []
       (App (Abs "f" (Abs "x" (App (Var "f")
                                         (App (Var "f")
                                              (Var "x")))))
            (Abs "g" (Abs "y" (App (Var "g")
                                         (App (Var "g")
                                              (Var "y"))))))
       (Abs "x" (Abs "y" (App (Var "x")
                              (App (Var "x")
                                   (App (Var "x")
                                        (App (Var "x")
                                             (Var "y")))))))
       ["x", "y", "f", "g"]
  -- twice twice
--  test [("twice", Abs "f" (Abs "x" (App (Var "f")
--                                        (App (Var "f")
--                                             (Var "x")))))]
--       (App (Var "twice")
--            (Var "twice"))
--       (Abs "x" (Abs "y" (App (Var "x")
--                              (App (Var "x")
--                                   (App (Var "x")
--                                        (App (Var "x")
--                                             (Var "y")))))))
  -- thrice thrice inc 100
--  test [("inc", Abs "x" (Add (Var "x") (Num 1))),
--        ("thrice", Abs "f" (Abs "x" (App (Var "f")
--                                         (App (Var "f")
--                                              (App (Var "f")
--                                                   (Var "x"))))))]
--       (App (App (App (Var "thrice")
--                      (Var "thrice"))
--                 (Var "inc"))
--            (Num 100))
--       (Num 127)
  putStrLn "All good"
