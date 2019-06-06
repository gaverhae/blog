module Main where

import qualified Data.List
import qualified Data.Set
import qualified Control.Monad

data E = Var String
       | Abs String E
       | App E E
       | Add E E
       | Num Int
       deriving (Eq, Show)

data Program = Program E Env
newtype Env = Env [(String, E)]

names = tail $ (Data.List.inits . repeat) ['a'..'z'] >>= sequence

usedNames :: Program -> Data.Set.Set String
usedNames (Program exp env) =
  used exp `Data.Set.union` predefined env
  where
  used exp = case exp of
    Var s -> Data.Set.singleton s
    Abs h e -> Data.Set.insert h $ used e
    App abs arg -> used abs `Data.Set.union` used arg
    Add a b -> used a `Data.Set.union` used b
    Num i -> Data.Set.empty
  predefined (Env e) = Data.Set.fromList (map fst e) `Data.Set.union` Data.Set.unions (map (used . snd) e)

-- rename all variables to a new, unused name, starting from the leaves and
-- moving up
prep :: Program -> Program
prep (Program exp (Env e)) =
  Program (fst (disambiguate exp avn)) (Env e)
  where un = usedNames (Program exp (Env e))
        avn = filter (`Data.Set.notMember` un) names
        r exp oldName newName =
          let rec e = r e oldName newName
          in case exp of
            Var s -> if s == oldName then Var newName else Var s
            Abs h b -> Abs h (rec b)
            App abs arg -> App (rec abs) (rec arg)
            Add t1 t2 -> Add (rec t1) (rec t2)
            Num i -> Num i
        disambiguate exp (n:ns) = case exp of
          Var s -> (Var s, n:ns)
          Abs h b -> (Abs n (r db h n), rs)
                     where (db, rs) = disambiguate b ns
          App abs arg -> (App dabs darg, rs')
                         where (dabs, rs) = disambiguate abs (n:ns)
                               (darg, rs') = disambiguate arg rs
          Add t1 t2 -> (Add dt1 dt2, rs')
                       where (dt1, rs) = disambiguate t1 (n:ns)
                             (dt2, rs') = disambiguate t2 rs
          Num i -> (Num i, n:ns)

eval :: Program -> E
eval p = eval' exp env
  where (Program exp (Env env)) = p
        eval' exp env = case exp of
          Var s -> find env s
          Abs h e -> Abs h $ eval' e $ (h, Var h):env
          App abs arg -> case (eval' abs env, eval' arg env) of
            (Abs h e, arg') -> eval' e $ (h, arg'):env
            (Var s, arg') -> App (Var s) arg'
            (a, b) -> App a b
          Add a b -> case (a', b') of
            (Num x, Num y) -> Num (x + y)
            _ -> Add a' b'
            where (a', b') = (eval' a env, eval' b env)
          Num i -> Num i

find env s =
  case env of
       [] -> Var s
       (n, e):tl -> if n == s then e else find tl s

initenv = []

test env e1 er e2 s = do
  Control.Monad.when (result /= e2) $ error $ "Error evaluating: \n(" ++ show e1 ++ ")\nto:\n(" ++ show result ++ ")\nwhile expecting:\n(" ++ show e2 ++ ")"
  Control.Monad.when (Data.Set.fromList s /= usedNames (Program e1 (Env env))) $ error $ "NameError in:\n" ++ show e1 ++  "\nfound\n" ++ show (usedNames (Program e1 (Env env))) ++ "\nexpecting\n" ++ show s
  let (Program er' _) = prep (Program e1 (Env env))
  Control.Monad.when (er' /= er) $ error $ "RenameError:\n(" ++ show e1 ++ ")\nyielded:\n(" ++ show er' ++ ")\ninstead of:\n(" ++ show er ++ ")"
  where result = eval (Program e1 (Env env))

main :: IO ()
main = do
  -- \x.x => \x.x
  test initenv
       (Abs "x" (Var "x"))
       (Abs "a" (Var "a"))
       (Abs "x" (Var "x"))
       ["x"]
  -- (\x.x) 2 => 2
  test initenv
       (App (Abs "x" (Var "x")) (Num 2))
       (App (Abs "a" (Var "a")) (Num 2))
       (Num 2)
       ["x"]
  -- (\x.x + 1) 10 => 11
  test initenv
       (App (Abs "x" (Add (Var "x") (Num 1))) (Num 10))
       (App (Abs "a" (Add (Var "a") (Num 1))) (Num 10))
       (Num 11)
       ["x"]
  -- (\x.x) (\y.y) => (\y.y)
  test initenv
       (App (Abs "x" (Var "x")) (Abs "y" (Var "y")))
       (App (Abs "a" (Var "a")) (Abs "b" (Var "b")))
       (Abs "y" (Var "y"))
       ["x", "y"]
  -- (\x.x) (\y.y) z => z
  test initenv
       (App (App (Abs "x" (Var "x"))
                 (Abs "y" (Var "y")))
            (Var "z"))
       (App (App (Abs "a" (Var "a"))
                 (Abs "b" (Var "b")))
            (Var "z"))
       (Var "z")
       ["x", "y", "z"]
  -- (\x.xy) z => xz
  test initenv
       (App (Abs "x" (App (Var "x") (Var "y")))
            (Var "z"))
       (App (Abs "a" (App (Var "a") (Var "y")))
            (Var "z"))
       (App (Var "z") (Var "y"))
       ["x", "y", "z"]
  -- (\xy.xy) (\z.a) => (\y.a)
  test initenv
       (App (Abs "x" (Abs "y" (App (Var "x") (Var "y"))))
            (Abs "z" (Var "a")))
       (App (Abs "b" (Abs "c" (App (Var "b") (Var "c"))))
            (Abs "d" (Var "a")))
       (Abs "y" (Var "a"))
       ["x", "y", "z", "a"]
  test initenv
       (App (App (Abs "x" (Abs "y" (App (Var "x") (Var "y"))))
                 (Abs "z" (Var "a")))
            (Num 1))
       (App (App (Abs "b" (Abs "c" (App (Var "b") (Var "c"))))
                 (Abs "d" (Var "a")))
            (Num 1))
       (Var "a")
       ["x", "y", "z", "a"]
  -- faking names with extra nesting
  -- inc = \x.(x + 1)
  -- inc 10
  test initenv
       (App (Abs "inc" (App (Var "inc") (Num 10)))
            (Abs "x" (Add (Var "x") (Num 1))))
       (App (Abs "a" (App (Var "a") (Num 10)))
            (Abs "b" (Add (Var "b") (Num 1))))
       (Num 11)
       ["inc", "x"]
  -- cheating: adding names to env
  test [("inc", Abs "x" (Add (Var "x") (Num 1)))]
       (App (Var "inc") (Num 10))
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
       (App (Var "thrice") (Var "inc"))
       (Abs "x" (Add (Add (Add (Var "x") (Num 1)) (Num 1)) (Num 1)))
       ["inc", "x", "thrice", "f"]
  -- (\x. inc x) 100 => 101
  test [("inc", Abs "x" (Add (Var "x") (Num 1)))]
       (App (Abs "x" (App (Var "inc")
                          (Var "x")))
            (Num 100))
       (App (Abs "a" (App (Var "inc")
                          (Var "a")))
            (Num 100))
       (Num 101)
       ["inc", "x"]
  -- (\x. inc (inc x)) 100 => 102
  test [("inc", Abs "x" (Add (Var "x") (Num 1)))]
       (App (Abs "x" (App (Var "inc")
                          (App (Var "inc")
                               (Var "x"))))
            (Num 100))
       (App (Abs "a" (App (Var "inc")
                          (App (Var "inc")
                               (Var "a"))))
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
       (App (Abs "a" (App (Var "inc")
                          (App (Var "inc")
                               (App (Var "inc")
                                    (Var "a")))))
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
       (App (Abs "a" (Abs "b" (App (Var "a")
                                         (App (Var "a")
                                              (Var "b")))))
            (Abs "c" (Abs "d" (App (Var "c")
                                         (App (Var "c")
                                              (Var "d"))))))
       (Abs "x" (Abs "y" (App (Var "x")
                              (App (Var "x")
                                   (App (Var "x")
                                        (App (Var "x")
                                             (Var "y")))))))
       ["x", "y", "f", "g"]
  -- (\xy.xxy) (\x.xy) (\x.xz) => \z.z
  test initenv
       (App (App (Abs "x" (Abs "y" (App (App (Var "x")
                                             (Var "x"))
                                        (Var "y"))))
                 (Abs "x" (App (Var "x")
                               (Var "y"))))
            (Abs "x" (App (Var "x")
                          (Var "z"))))
       (App (App (Abs "a" (Abs "b" (App (App (Var "a")
                                             (Var "a"))
                                        (Var "b"))))
                 (Abs "c" (App (Var "c")
                               (Var "y"))))
            (Abs "d" (App (Var "d")
                          (Var "z"))))
       (Abs "z" (Var "z"))
       ["x", "y", "z"]
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
