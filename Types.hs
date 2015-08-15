module Types where
import qualified Data.Map.Strict as M
import Data.List (intersperse)

type Env = M.Map Char Int

type State = (Env, [String], [String])

type Program = [Command]

data Command
  = Print  Expr
  | Read   Char
  | Assign Char Expr

instance Show Command where
  show (Read name)        = "read " ++ [name]
  show (Assign name expr) = "assign " ++ [name] ++ show expr
  show (Print expr)       = "print " ++ show expr

type Value    = Int
type Variable = Char

data Expr
  = Op Arith Expr Expr
  | Val Value
  | Var Variable

data Arith
  = Add
  | Sub
  | Mul
  | Div

instance Show Expr where
  show (Val val)  = show val
  show (Var name) = [name]
  show (Op Add a b)  = ('(':) . (++ ")") . concat . (intersperse "+") . (map show) $ [a, b]
  show (Op Sub a b)  = ('(':) . (++ ")") . concat . (intersperse "-") . (map show) $ [a, b]
  show (Op Mul a b)  = ('(':) . (++ ")") . concat . (intersperse "*") . (map show) $ [a, b]
  show (Op Div a b)  = ('(':) . (++ ")") . concat . (intersperse "/") . (map show) $ [a, b]
