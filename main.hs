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
step program (env, stdout, stdin) = do
	command <- getLine
	case command of
		"s" -> do
			case (execute (env, stdout, stdin) (head program)) of
				(env, stdout, stdin) -> do
					putStrLn . show $ stdout
					step (tail program) (env, stdout, stdin)

main :: IO ()
main = do
	contents <- readFile "test.np"
	step (parseProgram contents) (initialEnv, [], ["1", "10"])