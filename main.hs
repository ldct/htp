import System.Environment (getArgs)

import Ast
import Interpreter (execute, initialEnv)
import Compiler (compile)
import Parser (parseProgram)

--main :: IO ()
--main = do
--  args <- getArgs
--  case (args) of
--    ["c" ,filename] -> (putStr . compile . parseProgram) =<< readFile filename
--    ["i" ,filename] -> do
--      contents <- readFile filename
--      stdin <- getContents
--      case ((runProgram stdin) . parseProgram $ contents) of
--        (_, stdout, _) -> (putStr . unlines . reverse) stdout
--    _ -> putStrLn "Usage: `./main c filename` or `./main i filename`"

step :: Program -> (Env, [String], [String]) -> IO ()
step program ess = do
	command <- getLine
	case command of
		"forward" -> do
			step (tail program) (execute ess (head program))
		"stdin" -> do
			case ess of (_, _, stdin) -> putStrLn . show $ stdin
			step program ess
		"stdout" -> do
			case ess of (_, stdout, _) -> putStrLn . show $ stdout
			step program ess
		_ -> do
			putStrLn "???"
			step program ess

main :: IO ()
main = do
	contents <- readFile "test.np"
	step (parseProgram contents) (initialEnv, [], ["1", "10"])