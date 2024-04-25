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

data Logger l a where
  LBind :: Logger l a -> (a -> Logger l b) -> Logger l b
  LReturn :: a -> Logger l a
  LOutput :: l -> Logger l ()

instance Functor (Logger l) where fmap = liftM
instance Applicative (Logger l) where pure = return; (<*>) = ap
instance Monad (Logger l) where return = LReturn; (>>=) = LBind

runL :: Logger l a -> ([l], a)
runL = \case
  LReturn a -> ([], a)
  LBind ma f ->
    let (prev, a) = runL ma
        (next, b) = runL (f a)
    in (prev <> next, b)
  LOutput l -> ([l], ())

exampleL :: Logger Int Int
exampleL = do
  let a = 1
  LOutput a
  let b = a + 3
  LOutput b
  let c = b + 4
  LOutput c
  let result = 3 * c
  LOutput result
  return result

exampleF1 :: Logger String Double
exampleF1 = do
  let a = 5
  LOutput $ "[1]: a = " <> show a
  let b = a + 3
  LOutput $ "[1]: b = " <> show b
  let c = b + 5
  LOutput $ "[1]: c = " <> show c
  let i = b * c
  LOutput $ "[1]: i = " <> show i
  return $ i

exampleF2 :: Logger String Double
exampleF2 = do
  let a = 1
  LOutput $ "[2]: a = " <> show a
  let b = 2
  LOutput $ "[2]: b = " <> show b
  let c = 1
  LOutput $ "[2]: c = " <> show c
  let b2 = b * b
  LOutput $ "[2]: b2 = " <> show b2
  let ac = a * c
  LOutput $ "[2]: ac = " <> show ac
  let delta = b2 - 4 * ac
  LOutput $ "[2]: delta = " <> show delta
  let rac = delta ** 0.5
  LOutput $ "[2]: rac = " <> show rac
  return ((rac - b) / (2 * a))

runFiber :: [Logger l a] -> ([l], [a])
runFiber ms = loop ms [] []
  where
  step :: Logger l a -> ([l], Logger l a)
  step = \case
    LReturn a -> ([], LReturn a)
    LOutput l -> ([l], LReturn ())
    LBind ma f -> let (l, ma') = step ma
                  in (l, LBind ma' f)
  loop :: [Logger l a] -> [l] -> [a] -> ([l], [a])
  loop [] log rets = (reverse log, rets)
  loop (ma:ms) log rets = case ma of
    LReturn a -> loop ms log (a:rets)
    LOutput l -> loop ms (l:log) rets
    LBind (LReturn a) f -> loop (ms <> [f a]) log rets
    LBind (LOutput l) f -> loop (ms <> [f ()]) (l:log) rets
    LBind ma f -> let (l, ma') = step ma
                  in loop (ms <> [LBind ma' f]) (l <> log) rets

instance Functor (State s) where fmap = liftM
instance Applicative (State s) where pure = return; (<*>) = ap
instance Monad (State s) where return = SReturn; (>>=) = SBind

data State s a where
  SBind :: State s a -> (a -> State s b) -> State s b
  SReturn :: a -> State s a
  SPut :: s -> State s ()
  SGet :: State s s

exampleS = do
  SPut 5
  a <- SGet
  let b = 42
  SPut (a + b)
  c <- SGet
  return $ c * a

runS :: s -> State s a -> (s, a)
runS old_state = \case
  SReturn a -> (old_state, a)
  SPut new_state -> (new_state, ())
  SGet -> (old_state, old_state)
  SBind ma f -> let (new_state, a) = runS old_state ma
                in runS new_state (f a)

instance Functor (Stack s) where fmap = liftM
instance Applicative (Stack s) where pure = return; (<*>) = ap
instance Monad (Stack s) where return = StReturn; (>>=) = StBind

data Stack s a where
  StBind :: Stack s a -> (a -> Stack s b) -> Stack s b
  StReturn :: a -> Stack s a
  StPush :: s -> Stack s ()
  StPop :: Stack s s

exampleSt = do
  a <- StPop
  b <- StPop
  StPush (a + b)

runSt :: [s] -> Stack s a -> ([s], a)
runSt stack = \case
  StReturn a -> (stack, a)
  StBind ma f -> let (new_stack, a) = runSt stack ma
                 in runSt new_stack (f a)
  StPop -> (tail stack, head stack)
  StPush s -> (s:stack, ())

main :: IO ()
main = do
  print $ run example
  print $ runSC exampleSC
  print $ runL exampleL
  print $ runFiber [exampleF1, exampleF2]
  print $ runS 0 exampleS
  print $ runSt [10, 20] exampleSt
  print $ runSt [13, 256] exampleSt
