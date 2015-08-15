-- replace head and tail with pattern matching to keep functions total
module Debugger where
import System.IO (hFlush, stdout)

import Ast (Program, Env)
import Interpreter (execute, initialEnv)
import Parser (commandParser, runParser, resolveError)

-- remaining program, executed program, env, output, input
type ProgramState = (Program, Program, Env, [String], [String])

debug :: Program -> [String] -> IO ()
debug program stdin = debugHelper [(program, [], initialEnv, [], stdin)]

debugHelper :: [ProgramState] -> IO ()
debugHelper allStates@(state:states) = do
  putStr "\27[34m> "
  hFlush stdout
  command <- getLine
  putStr "\27[0m"
  case command of
    "f" -> debugHelper ((stepForward state):states)
    "b" -> debugHelper states
    "p" -> do
      putStr (showCurrentPosition state)
      debugHelper allStates
    "io" -> do
      putStr (showCurrentIO state)
      debugHelper allStates
    "_" -> do
      putStr "???"
      debugHelper allStates

stepForward :: ProgramState -> ProgramState
stepForward ((next:rest), executed, env, stdout, stdin) = (rest, next:executed, newEnv, newStdout, newStdin)
  where
  (newEnv, newStdout, newStdin) = execute (env, stdout, stdin) next

stepBackward :: [ProgramState] -> [ProgramState]
stepBackward (last:rest) = rest

replaceLine :: ProgramState -> IO ProgramState
replaceLine ((next:rest), executed, env, stdout, stdin) = do
  newLine <- getLine
  return (((resolveError (runParser commandParser newLine)):rest), executed, env, stdout, stdin)

showCurrentPosition :: ProgramState -> String
showCurrentPosition (remaining, executed, _, _, _) = ((unlines . map show . reverse) executed) ++ "------------\n" ++ ((unlines . map show) remaining)

showCurrentIO :: ProgramState -> String
showCurrentIO (_, _, _, stdout, stdin) = unlines ["stdin: ", show stdin, "stdout: ", (show . reverse) stdout]
