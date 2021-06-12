module Main where

import Control.Monad (ap, liftM)
import Data.Char (chr, ord)

data Exec a where
  Bind :: Exec a -> (a -> Exec b) -> Exec b
  Return :: a -> Exec a
  MoveRight :: Exec ()
  MoveLeft :: Exec ()
  Increment :: Exec ()
  Decrement :: Exec ()
  Input :: Exec ()
  Output :: Exec ()
  Get :: Exec Int

instance Functor Exec where fmap = liftM
instance Applicative Exec where pure = return; (<*>) = ap
instance Monad Exec where return = Return; (>>=) = Bind

parse :: String -> Exec ()
parse = \case
  ('>':xs) -> MoveRight >> parse xs
  ('<':xs) -> MoveLeft >> parse xs
  ('+':xs) -> Increment >> parse xs
  ('-':xs) -> Decrement >> parse xs
  ('.':xs) -> Output >> parse xs
  (',':xs) -> Input >> parse xs
  ('[':xs) -> let (till, after) = matching 0 ([],xs)
                  loop = do
                    v <- Get
                    if v == 0
                    then parse after
                    else do
                      parse till
                      loop
              in loop
  (_:xs) -> parse xs
  [] -> Return ()
  where
  matching :: Int -> (String, String) -> (String, String)
  matching 0 (till, ']':after) = (reverse till, after)
  matching n (till, ']':after) = matching (n - 1) (']':till, after)
  matching n (till, '[':after) = matching (n + 1) ('[':till, after)
  matching n (till, a:after) = matching n (a:till, after)
  matching n (till, []) = undefined

run :: String -> String -> String
run program input =
  toString $ exec (parse program) (0, [], fromString input) (\_ _ -> [])
  where
  toString :: [Int] -> String
  toString = map chr
  fromString :: String -> [Int]
  fromString = map ord
  exec :: Exec a -> (Int, [(Int, Int)], [Int]) -> (a -> (Int, [(Int, Int)], [Int]) -> [Int]) -> [Int]
  exec m state@(pointer, memory, input) cont = case m of
    Bind prev step -> exec prev state (\a state -> exec (step a) state cont)
    Return a -> cont a state
    MoveRight -> cont () (pointer + 1, memory, input)
    MoveLeft -> cont () (pointer - 1, memory, input)
    Increment -> cont () (pointer, update memory pointer (+1), input)
    Decrement -> cont () (pointer, update memory pointer (subtract 1), input)
    Get -> cont (get memory pointer) state
    Input -> cont () (pointer, update memory pointer (\_ -> (head input)), tail input)
    Output -> get memory pointer : cont () state
  update :: [(Int, Int)] -> Int -> (Int -> Int) -> [(Int, Int)]
  update [] p f = [(p, f 0)]
  update ((k,v):mem) p f = if k == p
                           then (k, f v) : mem
                           else (k, v) : update mem p f
  get :: [(Int, Int)] -> Int -> Int
  get [] p = 0
  get ((k,v):mem) p = if k == p
                      then v
                      else get mem p

main :: IO ()
main = do
  print $ run "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++." ""
