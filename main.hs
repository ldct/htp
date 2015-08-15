import System.Environment (getArgs)

import Ast
import Interpreter (execute, initialEnv)
import Compiler (compile)
import Parser (parseProgram)
import System.IO

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

main :: IO ()
main = do
	contents <- readFile "test.np"
	step (parseProgram contents) [] (initialEnv, [], ["1", "10"])