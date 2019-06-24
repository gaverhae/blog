module Main where

import qualified Data.List
import qualified Data.Set
import qualified Control.Monad
import qualified Control.Monad.State as State

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
  where used exp = case exp of
          Var s -> Data.Set.singleton s
          Abs h e -> Data.Set.insert h $ used e
          App abs arg -> used abs `Data.Set.union` used arg
          Add a b -> used a `Data.Set.union` used b
          Num i -> Data.Set.empty
        predefined e = Data.Set.fromList (map fst e) `Data.Set.union` Data.Set.unions (map (used . snd) e)

disambiguate :: E -> [String] -> (E, [String])
disambiguate exp (n:ns) =
  case exp of
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

find env s = case env of
  [] -> Var s
  (n, e):tl -> if n == s then e else find tl s

-- Brute force renaming, no monad

evalBruteForce :: E -> [(String, E)] -> E
evalBruteForce exp env =
  fst $ eval exp env avn
  where
    avn = filter (`Data.Set.notMember` un) names
    un = usedNames (Program exp env)
    eval exp env avn = case exp of
      Var s -> (find env s, avn)
      Abs h b -> (Abs h rb, rem)
        where (rb, rem) = eval b ((h, Var h):env) avn
      App abs arg -> case (dabs, rarg) of
        (Abs h b, arg') -> eval b ((h, rarg):env) rem''
        (a, b) -> (App a b, rem'')
        where (rabs, rem) = eval abs env avn
              (dabs, rem') = disambiguate rabs rem
              (rarg, rem'') = eval arg env rem'
      Add t1 t2 -> case (rt1, rt2) of
        (Num x, Num y) -> (Num (x + y), rem')
        _ -> (Add rt1 rt2, rem')
        where (rt1, rem) = eval t1 env avn
              (rt2, rem') = eval t2 env rem
      Num i -> (Num i, avn)

-- Implement rename with a State monad

evalMonad :: E  -> [(String, E)] -> E
evalMonad exp env =
  State.evalState (evalM exp env) avn
  where avn = filter (`Data.Set.notMember` un) names
        un = usedNames (Program exp env)

evalM :: E -> [(String, E)] -> State.State [String] E
evalM exp env = case exp of
  Var s -> return $ find env s
  Abs h b -> do
    rb <- evalM b ((h, Var h):env)
    return $ Abs h rb
  App abs arg -> do
    rabs <- evalM abs env
    dabs <- disambiguateM rabs
    rarg <- evalM arg env
    case (dabs, rarg) of
      (Abs h b, arg') -> evalM b ((h, rarg):env)
      (a, b) -> return $ App a b
  Add t1 t2 -> do
    rt1 <- evalM t1 env
    rt2 <- evalM t2 env
    return $ case (rt1, rt2) of
      (Num x, Num y) -> Num (x + y)
      _ -> Add rt1 rt2
  Num i -> return $ Num i

disambiguateM :: E -> State.State [String] E
disambiguateM exp = case exp of
  Var s -> return $ Var s
  Abs oldName oldBody -> do
    state <- State.get
    case state of
      [] -> error "Cannot happen, but necessary to make compiler happy"
      newName:freeNames -> do
        State.put freeNames
        newBody <- disambiguateM oldBody
        return $ Abs newName (renameIn newBody oldName newName)
  App abs arg -> do
    dabs <- disambiguateM abs
    darg <- disambiguateM arg
    return $ App dabs darg
  Add t1 t2 -> do
    dt1 <- disambiguateM t1
    dt2 <- disambiguateM t2
    return $ Add dt1 dt2
  Num i -> return $ Num i
  where renameIn exp oldName newName =
          let rec e = renameIn e oldName newName
          in case exp of
            Var s -> if s == oldName then Var newName else Var s
            Abs h b -> Abs h (rec b)
            App abs arg -> App (rec abs) (rec arg)
            Add t1 t2 -> Add (rec t1) (rec t2)
            Num i -> Num i

-- De Bruijn indices

data DBE = DBFree String
         | DBVar Int
         | DBAbs String DBE
         | DBApp DBE DBE
         | DBAdd DBE DBE
         | DBNum Int
         deriving (Eq, Show)

evalDeBruijn :: E -> [(String, E)] -> E
evalDeBruijn exp env = (toE 0 . eval . toDB . flattenEnv env) exp
  where flattenEnv env exp = case env of
          [] -> exp
          (name, value):xs -> flattenEnv xs $ App (Abs name exp) value
        toDB exp = case exp of
          Var s -> DBFree s
          Abs h b -> DBAbs h (subst h (toDB b) 1)
          App abs arg -> DBApp (toDB abs) (toDB arg)
          Add t1 t2 -> DBAdd (toDB t1) (toDB t2)
          Num i -> DBNum i
          where subst name dexp depth = case dexp of
                  DBFree s | s == name -> DBVar depth
                  DBFree s -> DBFree s
                  DBVar i -> DBVar i
                  DBAbs h b -> DBAbs h (subst name b (depth + 1))
                  DBApp abs arg -> DBApp (subst name abs depth)
                                         (subst name arg depth)
                  DBAdd t1 t2 -> DBAdd (subst name t1 depth)
                                       (subst name t2 depth)
                  DBNum i -> DBNum i
        eval dexp = case dexp of
          DBFree s -> DBFree s
          DBVar i -> DBVar i
          DBAbs h b -> DBAbs h (eval b)
          DBApp abs arg -> case (eval abs, eval arg) of
            (DBAbs h b, arg) -> eval $ subst arg b 1
            (abs, arg) -> DBApp abs arg
          DBAdd t1 t2 -> case (eval t1, eval t2) of
            (DBNum a, DBNum b) -> DBNum (a + b)
            (t1, t2) -> DBAdd t1 t2
          DBNum i -> DBNum i
          where subst rep dexp depth = case dexp of
                  DBFree s -> DBFree s
                  DBVar i | i > depth -> DBVar (i - 1)
                  DBVar i | i < depth -> DBVar i
                  DBVar i | i == depth -> offsetFree rep depth 0
                    where offsetFree dexp offset depth = case dexp of
                            DBFree s -> DBFree s
                            DBVar i | i > depth -> DBVar $ i + offset -1
                            DBVar i -> DBVar i
                            DBAbs h b -> DBAbs h (offsetFree b offset (depth + 1))
                            DBApp abs arg -> DBApp (offsetFree abs offset depth)
                                                   (offsetFree arg offset depth)
                            DBAdd t1 t2 -> DBAdd (offsetFree t1 offset depth)
                                                 (offsetFree t2 offset depth)
                            DBNum i -> DBNum i
                  DBAbs h b -> DBAbs h (subst rep b (depth + 1))
                  DBApp abs arg -> DBApp (subst rep abs depth)
                                         (subst rep arg depth)
                  DBAdd t1 t2 -> DBAdd (subst rep t1 depth)
                                       (subst rep t2 depth)
                  DBNum i -> DBNum i
        toE depth dexp = case dexp of
          DBFree s -> Var s
          DBVar i -> Var $ show i
          DBAbs h b -> let u = (h ++ "_" ++ show depth) in
                           Abs u (toE (depth + 1) (subst u b 1))
          DBApp abs arg -> App (toE depth abs) (toE depth arg)
          DBAdd t1 t2 -> Add (toE depth t1) (toE depth t2)
          DBNum i -> Num i
          where subst name dexp depth = case dexp of
                  DBFree s -> DBFree s
                  DBVar i | i == depth -> DBFree name
                  DBVar i -> DBVar i
                  DBAbs h b -> DBAbs h (subst name b (depth + 1))
                  DBApp abs arg -> DBApp (subst name abs depth)
                                         (subst name arg depth)
                  DBAdd t1 t2 -> DBAdd (subst name t1 depth)
                                       (subst name t2 depth)
                  DBNum i -> DBNum i

alpha :: E -> E -> Bool
alpha e1 e2 =
  de1 == de2
  where un = usedNames (Program e1 []) `Data.Set.union` usedNames (Program e2 [])
        avn = filter (`Data.Set.notMember` un) names
        (de1, _) = disambiguate e1 avn
        (de2, _) = disambiguate e2 avn

initenv = []

test env e1 e2 = do
  failOn evalBruteForce "evalBruteForce"
  failOn evalMonad "evalMonad"
  failOn evalDeBruijn "evalDeBruijn"
  where failOn f n =
          let result = f e1 env in
            Control.Monad.unless (result `alpha` e2) $ error $ "Error evaluating: \n(" ++ show e1 ++ ")\n with " ++ n ++ "to:\n(" ++ show result ++ ")\nwhile expecting:\n(" ++ show e2 ++ ")"

main :: IO ()
main = do

  -- \x.x => \x.x
  test initenv
       (Abs "x" (Var "x"))
       (Abs "y" (Var "y"))

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
