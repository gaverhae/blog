module Main where

data E = Var String
       | Abs String E
       | App E E
       | Add E E
       | Num Int
       deriving (Eq)

-- \x.x
iden = Abs "x" (Var "x")

-- (\x.x) 2
id2 = App (Abs "x" (Var "x")) (Num 2)

eval :: E -> (String -> E) -> E
eval exp env = case exp of
                    Var s -> env s
                    Abs s e -> exp
                    App abs arg -> undefined
                    Add a b -> undefined
                    Num i -> Num i

initenv var = error $ "Could not find " ++ var

assert x = if x then return () else error "bad"

main :: IO ()
main = do
  assert $ 1 == 1
  assert $ (eval iden initenv) == iden
  putStrLn "hello"
