module Main where

import Control.Monad (liftM, ap)

newtype State s a = State (s -> (s, a))

-- magic incantation to get `do` syntax
instance Functor (State s) where fmap = liftM
instance Applicative (State s) where pure = return; (<*>) = ap

instance Monad (State s) where
  return a = State $ \s -> (s, a)
  (State ma) >>= f = State $ \s0 ->
    let (s1, a) = ma s0
    in let State (g) = f a
    in g s1

put :: s -> State s ()
put s = State $ \_ -> (s, ())

get :: State s s
get = State $ \s -> (s, s)

run :: State s a -> s -> (s, a)
run (State f) a = f a

data Stack = Empty | Stack { top :: Int, rest :: Stack } deriving Show

push :: Int -> State Stack ()
push i = do
  old_stack <- get
  put (Stack i old_stack)

pop :: State Stack Int
pop = do
  old_stack <- get
  put (rest old_stack)
  return (top old_stack)

main :: IO ()
main = do
  let stack_computation =
        do
          a <- pop
          b <- pop
          push (a + b)
  let init_1 = Stack 10 (Stack 20 Empty)
  let init_2 = Stack 13 (Stack 256 Empty)

  print $ run stack_computation init_1
  print $ run stack_computation init_2
