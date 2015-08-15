module Interpreter where
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M

import Ast

runProgram :: Program -> (Env, [String])
runProgram program = foldl execute (M.empty, []) program

execute :: (Env, [String]) -> Command -> (Env, [String])
execute (env, stdout) (Assign name val) = (M.insert name (eval env val) env, stdout)
execute (env, stdout) (Print expr)      = (env, ((show . (eval env)) expr):stdout)
--execute env output (Read name)       = getLine >>= (\input -> execute env (Assign name (Val (read input))))

eval :: Env -> Expr -> Int
eval _   (Val val)  = val
eval env (Var name) = fromMaybe (error $ "Var (" ++ [name] ++ ") not found") (M.lookup name env)
eval env (Add a b)  = (eval env a) + (eval env b)
eval env (Sub a b)  = (eval env a) - (eval env b)
eval env (Mul a b)  = (eval env a) * (eval env b)
eval env (Div a b)  = (eval env a) `div` (eval env b)
