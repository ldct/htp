import System.Environment (getArgs)

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
      stdin <- getContents
      case ((runProgram stdin) . parseProgram $ contents) of
        (_, stdout, _) -> (putStr . unlines . reverse) stdout
    _ -> putStrLn "Usage: `./main c filename` or `./main i filename`"
