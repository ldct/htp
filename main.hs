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
    ["i" ,filename] -> do
      contents <- readFile filename
      case (runProgram . parseProgram $ contents) of
        (_, stdout) -> (putStr . unlines . reverse) stdout
    _ -> putStrLn "Usage: `./main c filename` or `./main i filename`"
