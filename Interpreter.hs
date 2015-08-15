module Interpreter where
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M

import Ast

runProgram :: String -> Program -> (Env, [String], [String])
runProgram stdin program = foldl execute (M.empty, [], lines stdin) program

execute :: (Env, [String], [String]) -> Command -> (Env, [String], [String])
execute (env, stdout, stdin) (Assign name val) = (M.insert name (eval env val) env, stdout, stdin)
execute (env, stdout, stdin) (Print expr)      = (env, ((show . (eval env)) expr):stdout, stdin)
execute (env, stdout, x:xs) (Read name)        = execute (env, stdout, xs) (Assign name (Val (read x)))

eval :: Env -> Expr -> Int
eval _   (Val val)  = val
eval env (Var name) = fromMaybe (error $ "Var (" ++ [name] ++ ") not found") (M.lookup name env)
eval env (Add a b)  = (eval env a) + (eval env b)
eval env (Sub a b)  = (eval env a) - (eval env b)
eval env (Mul a b)  = (eval env a) * (eval env b)
eval env (Div a b)  = (eval env a) `div` (eval env b)
