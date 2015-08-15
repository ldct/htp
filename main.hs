import System.Environment (getArgs)

import Ast
import Interpreter (runProgram)
import Compiler (compile)
import Parser (parseProgram)

main :: IO ()
main = do
  args <- getArgs
  case (args) of
    ["c"] -> putStr . compile $ prog
    ["i"] -> case (runProgram prog) of
      (_, stdout) -> (putStr . unlines . reverse) stdout
    _     -> putStrLn "Usage: `./main c` or `./main i`"

prog :: Program
prog = parseProgram . unlines $ ["assign a 3 * 2", "assign b a + 1", "print a + b", "print b"]
