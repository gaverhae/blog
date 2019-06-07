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

data Program = Program E [(String, E)]

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
  predefined e = Data.Set.fromList (map fst e) `Data.Set.union` Data.Set.unions (map (used . snd) e)

disambiguate :: E -> [String] -> (E, [String])
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
  where r exp oldName newName =
          let rec e = r e oldName newName
          in case exp of
            Var s -> if s == oldName then Var newName else Var s
            Abs h b -> Abs h (rec b)
            App abs arg -> App (rec abs) (rec arg)
            Add t1 t2 -> Add (rec t1) (rec t2)
            Num i -> Num i

eval :: Program -> E
eval (Program exp env) = fst $ eval' exp env avn
  where avn = filter (`Data.Set.notMember` un) names
        un = usedNames (Program exp env)
        eval' exp env avn = case exp of
          Var s -> (find env s, avn)
          Abs h b -> (Abs h rb, rem)
                     where (rb, rem) = eval' b ((h, Var h):env) avn
          App abs arg -> case (dabs, rarg) of
            (Abs h b, arg') -> eval' b ((h, rarg):env) rem''
            (a, b) -> (App a b, rem'')
            where (rabs, rem) = eval' abs env avn
                  (dabs, rem') = disambiguate rabs rem
                  (rarg, rem'') = eval' arg env rem'
          Add t1 t2 -> case (rt1, rt2) of
            (Num x, Num y) -> (Num (x + y), rem')
            _ -> (Add rt1 rt2, rem')
            where (rt1, rem) = eval' t1 env avn
                  (rt2, rem') = eval' t2 env rem
          Num i -> (Num i, avn)

find env s = case env of
  [] -> Var s
  (n, e):tl -> if n == s then e else find tl s

initenv = []

test env e1 e2 = do
  Control.Monad.when (result /= e2) $ error $ "Error evaluating: \n(" ++ show e1 ++ ")\nto:\n(" ++ show result ++ ")\nwhile expecting:\n(" ++ show e2 ++ ")"
  where result = eval (Program e1 env)

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

  -- (\xy. (\x.xx)y) -> (\xy.yy)
  test initenv
       (Abs "x" (Abs "y" (App (Abs "x" (App (Var "x") (Var "x")))
                              (Var "y"))))
       (Abs "x" (Abs "y" (App (Var "y") (Var "y"))))

  -- (\x.x) (\y.y) z => z
  test initenv
       (App (App (Abs "x" (Var "x"))
                 (Abs "y" (Var "y")))
            (Var "z"))
       (Var "z")

  -- (\x.xy) z => zy
  test initenv
       (App (Abs "x" (App (Var "x") (Var "y")))
            (Var "z"))
       (App (Var "z") (Var "y"))

  -- (\xy.xy) (\z.a) => (\y.a)
  test initenv
       (App (Abs "x" (Abs "y" (App (Var "x") (Var "y"))))
            (Abs "z" (Var "a")))
       (Abs "c" (Var "a"))

  -- (\xy.xy) (\z.a) 1 => a
  test initenv
       (App (App (Abs "x" (Abs "y" (App (Var "x") (Var "y"))))
                 (Abs "z" (Var "a")))
            (Num 1))
       (Var "a")

  -- (\xy.xxy) (\x.xy) (\x.xz) => y y (\x.xz)
  test initenv
       (App (App (Abs "x" (Abs "y" (App (App (Var "x")
                                             (Var "x"))
                                        (Var "y"))))
                 (Abs "x" (App (Var "x")
                               (Var "y"))))
            (Abs "x" (App (Var "x")
                          (Var "z"))))
       (App (App (Var "y") (Var "y")) (Abs "x" (App (Var "x") (Var "z"))))

  -- (\xyz.xz(yz)) (\mn.m) (\p.p)
  test initenv
       (App (App (Abs "x" (Abs "y" (Abs "z" (App (App (Var "x")
                                                      (Var "z"))
                                                 (App (Var "y")
                                                      (Var "z"))))))
                 (Abs "m" (Abs "n" (Var "m"))))
            (Abs "p" (Var "p")))
       (Abs "h" (Var "h"))

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
       (Abs "b" (Add (Add (Add (Var "b") (Num 1)) (Num 1)) (Num 1)))

  -- (\x. inc x) 100 => 101
  test [("inc", Abs "x" (Add (Var "x") (Num 1)))]
       (App (Abs "x" (App (Var "inc")
                          (Var "x")))
            (Num 100))
       (Num 101)

  -- (\x. inc (inc x)) 100 => 102
  test [("inc", Abs "x" (Add (Var "x") (Num 1)))]
       (App (Abs "x" (App (Var "inc")
                          (App (Var "inc")
                               (Var "x"))))
            (Num 100))
       (Num 102)

  -- (\x. inc (inc (inc x))) 100 => 103
  test [("inc", Abs "x" (Add (Var "x") (Num 1)))]
       (App (Abs "x" (App (Var "inc")
                          (App (Var "inc")
                               (App (Var "inc")
                                    (Var "x")))))
            (Num 100))
       (Num 103)

  -- thrice inc 100
  test [("inc", Abs "x" (Add (Var "x") (Num 1))),
        ("thrice", Abs "f" (Abs "x" (App (Var "f")
                                         (App (Var "f")
                                              (App (Var "f")
                                                   (Var "x"))))))]
       (App (App (Var "thrice") (Var "inc"))
            (Num 100))
       (Num 103)

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

  -- twice twice
  test []
       (App (Abs "f" (Abs "x" (App (Var "f")
                                         (App (Var "f")
                                              (Var "x")))))
            (Abs "g" (Abs "y" (App (Var "g")
                                         (App (Var "g")
                                              (Var "y"))))))
       (Abs "b" (Abs "d" (App (Var "b")
                              (App (Var "b")
                                   (App (Var "b")
                                        (App (Var "b")
                                             (Var "d")))))))

  -- twice twice
  test []
       (App (Abs "f" (Abs "x" (App (Var "f")
                                   (App (Var "f")
                                        (Var "x")))))
            (Abs "f" (Abs "x" (App (Var "f")
                                   (App (Var "f")
                                        (Var "x"))))))
       (Abs "b" (Abs "d" (App (Var "b")
                              (App (Var "b")
                                   (App (Var "b")
                                        (App (Var "b")
                                             (Var "d")))))))

  -- twice twice
  test []
       (App (Abs "twice" (App (Var "twice")
                              (Var "twice")))
            (Abs "f" (Abs "x" (App (Var "f")
                                   (App (Var "f")
                                        (Var "x"))))))
       (Abs "c" (Abs "e" (App (Var "c")
                              (App (Var "c")
                                   (App (Var "c")
                                        (App (Var "c")
                                             (Var "e")))))))

  -- twice twice
  test [("twice", Abs "f" (Abs "x" (App (Var "f")
                                        (App (Var "f")
                                             (Var "x")))))]
       (App (Var "twice")
            (Var "twice"))
       (Abs "b" (Abs "d" (App (Var "b")
                              (App (Var "b")
                                   (App (Var "b")
                                        (App (Var "b")
                                             (Var "d")))))))

  -- thrice thrice inc 100
  test [("inc", Abs "x" (Add (Var "x") (Num 1))),
        ("thrice", Abs "f" (Abs "x" (App (Var "f")
                                         (App (Var "f")
                                              (App (Var "f")
                                                   (Var "x"))))))]
       (App (App (App (Var "thrice")
                      (Var "thrice"))
                 (Var "inc"))
            (Num 100))
       (Num 127)
  putStrLn "All good"
