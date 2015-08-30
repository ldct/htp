import System.Environment (getArgs)

import Interpreter        (runProgram)
import Compiler           (compile)
import Debugger           (debug)
import Parser             (parseProgram)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["c", filename] ->
      (putStr . compile . parseProgram) =<< readFile filename

    ["i", filename] -> do
      contents <- readFile filename
      stdin    <- getContents
      case (runProgram stdin . parseProgram $ contents) of
        (_, stdout, _) -> (putStr . unlines . reverse) stdout

    ["d", filename] -> do
      contents <- readFile filename
      debug (parseProgram contents) ["1", "10"]

    _ -> putStrLn "Usage: `./main [cdi] filename`"
