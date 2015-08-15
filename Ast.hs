module Ast where
import qualified Data.Map.Strict as M
import Data.List (intersperse)

type Env = M.Map Char Int

type Program = [Command]

data Command
  = Print Expr
  | Read Char
  | Assign Char Expr

instance Show Command where
  show (Read name) = "read " ++ [name]
  show (Assign name expr) = "assign " ++ [name] ++ show expr
  show (Print expr) = "print " ++ show expr

data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Val Int
  | Var Char

instance Show Expr where
  show (Val val)  = show val
  show (Var name) = [name]
  show (Add a b)  = ('(':) . (++ ")") . concat . (intersperse "+") . (map show) $ [a, b]
  show (Sub a b)  = ('(':) . (++ ")") . concat . (intersperse "-") . (map show) $ [a, b]
  show (Mul a b)  = ('(':) . (++ ")") . concat . (intersperse "*") . (map show) $ [a, b]
  show (Div a b)  = ('(':) . (++ ")") . concat . (intersperse "/") . (map show) $ [a, b]
