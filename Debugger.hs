module Debugger where
import Ast (Program, Env)
import Interpreter (execute, initialEnv)
import System.IO (hFlush, stdout)

debug :: Program -> [String] -> IO ()
debug program stdin = step program [] (initialEnv, [], stdin)

step :: Program -> Program -> (Env, [String], [String]) -> IO ()
step program executed_program ess = do
  putStr "\27[34m> "
  hFlush stdout
  command <- getLine
  case command of
    "forward" -> do
      step (tail program)
         ([head program] ++ executed_program)
         (execute ess (head program))
    "io" -> case ess of
      (_, stdout, stdin) -> do
        putStr "\27[0m"
        putStr "stdin: "
        putStrLn . show $ stdin
        putStr "stdout: "
        putStrLn . show $ stdout
        step program executed_program ess
    "program" -> do
      putStr "\27[0m"
      putStr . unlines . map show . reverse $ executed_program
      putStrLn "------------"
      putStr . unlines . map show $ program
      step program executed_program ess
    _ -> do
      putStrLn "???"
      step program executed_program ess
