# Programming in Haskell

My notes on reading [Programming in Haskell][0].

[0]: https://www.amazon.co.uk/Programming-Haskell-Graham-Hutton/dp/0521692695

# Chapter 9 - Interactive programs

## 9.1 - Interaction

A _batch_ program knows all of its inputs at startup. By contrast, an
_interactive_ program can take additional input while it's running, as well as
produce incremental output.

## 9.2 - The IO type

In Haskell, an interactive program is a pure function that takes the "current
state of the world" as its argument, and produces a modified world as its
result. Like `Parser`, `IO` can return a result alongside the new world:

```haskell
type IO a = World -> (a, World)
```

## 9.3 - Basic actions

```haskell
getChar :: IO Char
-- native implementation

putChar :: Char -> IO ()
-- native implementation

return :: a -> IO a
-- native implementation
```

`return` provides a bridge from pure values to `IO` values. There is no way
back.

## 9.4 - Sequencing

```haskell
(>>=) :: IO a -> (a -> IO b) -> IO b
```

Thus we can use `do` notation.

## 9.5 - Derived primitives

```haskell
getLine :: IO String
getLine = do
  x <- getChar
  if x == '\n'
  then do
    return []
  else do
    xs <- getLine
    return (x:xs)

putStr :: String -> IO ()
putStr [] = return ()
putStr (x:xs) = do
  putChar x
  putStr xs

putStrLn :: String -> IO ()
putStrLn xs = do
  putStr xs
  putChar '\n'

beep :: IO ()
beep = putStr "\BEL"

cls :: IO ()
cls = putStr "\ESC[2J"

newtype Pos = (Int, Int)
  deriving Eq

goto :: Pos -> IO ()
goto (Pos (x, y)) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat (Pos p) xs = do
  goto p
  putStr xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a:as) = do
  a
  seqn as
```

## 9.6 - Calculator

```haskell

box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]

buttons :: [Char]
buttons = standard ++ extra
  where standard = "qcd=123+456-789*0()/"
        extra = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = seqn [ writeat (1, y) xs | (y, xs) <- zip [1..13] box]

display :: String -> IO ()
display xs = do
  writeat (3, 2) "             "
  writeat (3, 2) (reverse (take 13 (reverse xs)))

calc :: String -> IO ()
calc xs = do
  display xs
  c <- getCh
  if elem c buttons
  then do
    process c xs
  else do
    beep
    calc xs

process :: Char -> String -> IO ()
process c xs
  | elem c "qQ\ESC" = quit
  | elem c "dD\BS\DEL" = delete xs
  | elem c "=\n" = eval xs
  | elem c "cC" = clear
  | otherwise = press c xs

quit :: IO ()
quit = goto (1, 14)

delete :: String -> IO ()
delete " " = calc " "
delete xs = calc (init xs)

eval :: String -> IO ()
eval xs = case parse expr xs of
  [(n, " ")] -> calc (show n)
  _ -> do
     beep
     calc xs

clear :: IO ()
clear = calc " "

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do
  cls
  showbox
  clear
```

## 9.7 - Game of life

The game is played on a board; each cell is either alive of dead. The board is
assumed to wrap around. At each step:

- A living cell survives if it has 2 or 3 live neighbours.
- A dead cell comes back to life if it has exactly 3 live neighbours.

All cells update at the same time, and in all other cases the cell dies.

```haskell
width :: Int
width = 5

height :: Int
height = 5

newtype Board = Board [Pos]
  deriving Eq

glider :: Board
glider = Board [Pos (4, 2), Pos (2, 3), Pos (4, 3), Pos (3, 4), Pos (4, 4)]

showcells :: Board -> IO ()
showcells (Board b) = seqn [ writeat p "O" | p <- b ]

isAlive :: Board -> Pos -> Bool
isAlive (Board b) p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (Pos (x, y)) = map wrap [Pos (x - 1, y - 1), Pos (x, y - 1), Pos (x + 1, y - 1),
                                 Pos (x - 1, y),                     Pos (x + 1, y),
                                 Pos (x - 1, y + 1), Pos (x, y + 1), Pos (x + 1, y + 1)]

wrap :: Pos -> Pos
wrap (Pos (x, y)) = Pos (((x - 1) `mod` width) + 1,
                         ((y - 1) `mod` height) + 1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2, 3]]

births :: Board -> [Pos]
births b = [p | p <- rmdups (concat (map (neighbs b)))
              , isEmpty b p
              , liveneighbs b p == 3]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

nextgen :: Board -> Board
nextgen b = Board (survivors b ++ births b)

life :: Board -> IO ()
life b = do
  cls
  showcells b
  wait 5000
  life (nextgen b)

wait :: Int -> IO ()
wait n = seqn [ return () | _ <- [1..n]]
```

## 9.8 - Chapter remarks

:shrug:

## 9.9 - Exercises

> 1. Define an action `readLine :: IO String` that behaves in the same way as
>    `getLine`, except that it also permits the delete key to be used to remove
>    characters. Hint: the delete character is ’\DEL’, and the control string
>    for moving the cursor back one character is "\ESC[1D".

```haskell
readLine :: IO String
readLine = loop []
  where
  loop :: String -> IO String
  loop line = do
    c <- getCh
    case c of
      '\n' -> do
        putChar c
        return line
      '\DEL' -> do
        case line of
          [] -> loop []
          xs -> do
            putStr "\ESC[1D \ESC[1D"
            loop (init line)
      _ -> do
        putChar c
        loop (line ++ [c])
```
