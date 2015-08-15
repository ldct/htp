module Interpreter where
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M

import Types

initialEnv :: Env
initialEnv = M.empty

runProgram :: String -> Program -> State
runProgram stdin program = foldl execute (M.empty, [], lines stdin) program

execute :: State -> Command -> State
execute (env, stdout, stdin) (Assign name val) = (M.insert name (eval env val) env, stdout, stdin)
execute (env, stdout, stdin) (Print expr)      = (env, ((show . (eval env)) expr):stdout, stdin)
execute (env, stdout, x:xs)  (Read name)       = execute (env, stdout, xs) (Assign name (Val (read x)))
execute (_, _, [])           (Read _)          = error "Ran out of input"

eval :: Env -> Expr -> Int
eval _   (Val val)  = val
eval env (Var name) = fromMaybe (error $ "Var (" ++ [name] ++ ") not found") (M.lookup name env)
eval env (Op Add a b)  = (eval env a) +     (eval env b)
eval env (Op Sub a b)  = (eval env a) -     (eval env b)
eval env (Op Mul a b)  = (eval env a) *     (eval env b)
eval env (Op Div a b)  = (eval env a) `div` (eval env b)
