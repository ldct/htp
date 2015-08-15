import System.Environment (getArgs)

import Ast
import Interpreter (runProgram)
import Compiler (compile)
import Parser (parseProgram)

main :: IO ()
main = do
  args <- getArgs
  case (args) of
    ["c" ,filename] -> (putStr . compile . parseProgram) =<< readFile filename
    ["i" ,filename] -> (runProgram . parseProgram) =<< readFile filename
    _     -> putStrLn "Usage: `./main c filename` or `./main i filename`"