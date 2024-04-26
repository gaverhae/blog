module Main where

import qualified Control.Monad

instance Functor WithFibers where fmap = Control.Monad.liftM
instance Applicative WithFibers where
  pure = return
  (<*>) = Control.Monad.ap
instance Monad WithFibers where return = Return; (>>=) = Bind

data WithFibers a where
  Bind :: WithFibers a -> (a -> WithFibers b) -> WithFibers b
  Return :: a -> WithFibers a

step_fibers :: WithFibers a -> WithFibers a
step_fibers = \case
  Bind (Return a) f -> f a
  Bind ma f -> Bind (step_fibers ma) f
  Return a -> Return a

exec_fibers :: [WithFibers a] -> [a]
exec_fibers = loop []
  where
  loop :: [a] -> [WithFibers a] -> [a]
  loop rets [] = rets
  loop rets (x:xs) = case x of
    Return a -> loop (a:rets) xs
    Bind ma f -> loop rets (xs <> [(step_fibers x)])

main :: IO ()
main = print $ exec_fibers [
  do
    let a = 5
    b <- return $ a + 3
    c <- return $ b + 5
    return $ c * b,
  do
    let a = 1
        b = 2
        c = 1
    b2 <- return $ b * b
    ac <- return $ a * c
    delta <- return $ b2 - 4 * ac
    rac <- return $ delta ** 0.5
    return $ ((- b) + rac) / (2 * a)]


