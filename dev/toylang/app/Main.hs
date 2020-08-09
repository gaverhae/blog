module Main where

import qualified GHC.IO.Handle as IO
import qualified GHC.IO.Handle.FD as IO

import qualified Direct
import qualified Parser
import qualified ParseTree
import qualified Value

assert :: (Eq a, Show a) => a -> a -> IO ()
assert actual expected =
  if actual == expected
  then do
      IO.hPutStr IO.stdout "."
      IO.hFlush IO.stdout
      return ()
  else
      error $ "\nExpected:\n" <> show expected <> "\nbut got:\n" <> show actual

testParser :: IO ()
testParser = do
  assert (Parser.read "2") (ParseTree.Int 2)
  assert (Parser.read "(2)") (ParseTree.App [ParseTree.Int 2])
  assert (Parser.read "(concat \"hello \" \"world!\")") (ParseTree.App [ParseTree.Symbol "concat", ParseTree.String "hello ", ParseTree.String "world!"])
  assert (Parser.read "(test 4 (let [my-count (fn [v] (cond (= [] v) 0 true (+ 1 (my-count (rest v)))))] (my-count [1 2 3 4])))")
    (ParseTree.App
      [(ParseTree.Symbol "test"),
       (ParseTree.Int 4),
       (ParseTree.App
         [(ParseTree.Symbol "let"),
          (ParseTree.Vector
            [(ParseTree.Symbol "my-count"),
             (ParseTree.App
               [(ParseTree.Symbol "fn"),
                (ParseTree.Vector [(ParseTree.Symbol "v")]),
                (ParseTree.App
                  [(ParseTree.Symbol "cond"),
                   (ParseTree.App
                     [(ParseTree.Symbol "="),
                      (ParseTree.Vector []),
                      (ParseTree.Symbol "v")]),
                   (ParseTree.Int 0),
                   (ParseTree.Boolean True),
                   (ParseTree.App
                     [(ParseTree.Symbol "+"),
                      (ParseTree.Int 1),
                      (ParseTree.App
                        [(ParseTree.Symbol "my-count"),
                         (ParseTree.App [(ParseTree.Symbol "rest"),
                                     (ParseTree.Symbol "v")])])])])])]),
          (ParseTree.App
            [(ParseTree.Symbol "my-count"),
             (ParseTree.Vector
               [(ParseTree.Int 1),
                (ParseTree.Int 2),
                (ParseTree.Int 3),
                (ParseTree.Int 4)])])])])

testDirect :: IO ()
testDirect = do
  assert (Direct.eval $ Parser.read "2") (Value.Int 2)
  assert (Direct.eval $ Parser.read "\"hello\"") (Value.String "hello")
  assert (Direct.eval $ Parser.read "true") (Value.Boolean True)
  assert (Direct.eval $ Parser.read "false") (Value.Boolean False)
  assert (Direct.eval $ Parser.read "[1 2 \"hello\" true]") (Value.Vector [(Value.Int 1), (Value.Int 2), (Value.String "hello"), (Value.Boolean True)])
  assert (Direct.eval $ Parser.read "+") (Value.primitivePlus)
  assert (Direct.eval $ Parser.read "(+ 1 2)") (Value.Int 3)
  assert (Direct.eval $ Parser.read "(cond false 1 true 2)") (Value.Int 2)

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.NoBuffering
  testParser
  testDirect
  IO.hPutStr IO.stdout "\nDone.\n"
