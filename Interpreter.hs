module Interpreter where
import Data.Maybe (fromMaybe)
import Control.Monad (foldM)
import qualified Data.Map.Strict as M

import Ast

runProgram :: Program -> IO ()
runProgram program = foldM execute M.empty program >> return ()

execute :: Env -> Command -> IO Env
execute env (Assign name val) = return $ M.insert name (eval env val) env
execute env (Print expr)      = (putStrLn . show . (eval env)) expr >> return env
--execute env (Read name)       = getLine >>= (\input -> return $ M.insert name (read input) env)
execute env (Read name)       = getLine >>= (\input -> execute env (Assign name (Val (read input))))

eval :: Env -> Expr -> Int
eval _   (Val val)  = val
eval env (Var name) = fromMaybe (error $ "Var (" ++ [name] ++ ") not found") (M.lookup name env)
eval env (Add a b)  = (eval env a) + (eval env b)
eval env (Sub a b)  = (eval env a) - (eval env b)
eval env (Mul a b)  = (eval env a) * (eval env b)
eval env (Div a b)  = (eval env a) `div` (eval env b)
