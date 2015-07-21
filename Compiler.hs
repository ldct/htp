module Compiler where
import Data.List (intersperse, nub)

import Ast

compile :: Program -> String
-- compile program = preProgram ++ (unlines . map ("  " ++ ) . concatMap ($ program)) [declareVars . findVars, map transformCommand] ++ postProgram
compile program = preProgram ++ (unlines . map ("  " ++ )) (declarations ++ mainProgram) ++ postProgram
  where
  preProgram :: String
  preProgram = "#include <stdio.h>\nint main() {\n"
  declarations :: [String]
  declarations = (declareVars . findVars) program
  mainProgram :: [String]
  mainProgram = map transformCommand program
  postProgram :: String
  postProgram = "}\n"

transformCommand :: Command -> String
transformCommand (Assign name val) = name:" = " ++ (transformExpr val) ++ ";"
transformCommand (Print expr)      = "printf(\"%d\\n\", " ++ (transformExpr expr) ++ ");"
transformCommand (Read name)       = "scanf(\"%d\", &" ++ name:");"

transformExpr :: Expr -> String
transformExpr (Val val)  = show val
transformExpr (Var name) = [name]
transformExpr (Add a b)  = concat . (intersperse "+") . (map transformExpr) $ [a, b]
transformExpr (Sub a b)  = concat . (intersperse "-") . (map transformExpr) $ [a, b]
transformExpr (Mul a b)  = concat . (intersperse "*") . (map transformExpr) $ [a, b]
transformExpr (Div a b)  = concat . (intersperse "/") . (map transformExpr) $ [a, b]

declareVars :: [Char] -> [String]
declareVars = map (\var -> "int " ++ [var] ++ ";") -- . findVars

findVars :: Program -> [Char]
findVars = nub . (concatMap findVarsCommand)
  where
  findVarsCommand :: Command -> [Char]
  findVarsCommand (Assign name val) = name:(findVarsExpr val)
  findVarsCommand (Print expr)      = findVarsExpr expr
  findVarsCommand (Read name)       = [name]
  findVarsExpr :: Expr -> [Char]
  findVarsExpr (Val _)    = []
  findVarsExpr (Var name) = [name]
  findVarsExpr (Add a b)  = concatMap findVarsExpr [a, b]
  findVarsExpr (Sub a b)  = concatMap findVarsExpr [a, b]
  findVarsExpr (Mul a b)  = concatMap findVarsExpr [a, b]
  findVarsExpr (Div a b)  = concatMap findVarsExpr [a, b]

