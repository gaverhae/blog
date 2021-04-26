module Main where

import Control.Monad (ap,liftM)

instance Functor MyMonad where fmap = liftM
instance Applicative MyMonad where pure = return; (<*>) = ap
instance Monad MyMonad where return = Return; (>>=) = Bind

data MyMonad a where
  Bind :: MyMonad a -> (a -> MyMonad b) -> MyMonad b
  Return :: a -> MyMonad a

example :: MyMonad Int
example = do
  a <- pure 15
  b <- pure 18
  return $ a + b

run :: MyMonad a -> a
run = \case
  Return a -> a
  Bind ma f -> run (f (run ma))

instance Functor ShortCircuit where fmap = liftM
instance Applicative ShortCircuit where pure = return; (<*>) = ap
instance Monad ShortCircuit where return = SCReturn; (>>=) = SCBind

data ShortCircuit a where
  SCBind :: ShortCircuit a -> (a -> ShortCircuit b) -> ShortCircuit b
  SCReturn :: a -> ShortCircuit a
  SCStop :: ShortCircuit a

exampleSC :: ShortCircuit Double
exampleSC = do
  r <- pure 5.0
  r <- pure $ r + 7.0
  r <- pure $ r - 2.0
  r <- sqr_inv r
  return $ r * 3.0
  where
  sqr_inv :: Double -> ShortCircuit Double
  sqr_inv a = do
    x <- pure a
    x <- sqrt x
    div 1.0 x
  sqrt :: Double -> ShortCircuit Double
  sqrt a = if a < 0.0 then SCStop else return $ a ** 0.5
  div :: Double -> Double -> ShortCircuit Double
  div a b = if b == 0 then SCStop else return $ a / b

runSC :: ShortCircuit a -> Maybe a
runSC = \case
  SCReturn a -> Just a
  SCBind ma f -> case runSC ma of
    Nothing -> Nothing
    Just a -> runSC (f a)
  SCStop -> Nothing

main :: IO ()
main = do
  print $ run example
  print $ runSC exampleSC
