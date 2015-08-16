module Types where
import qualified Data.Map.Strict as M
import Data.List (intersperse)

--type Value    = Int | Bool
type Value = Int

type Variable = Char -- ['a'..'z'] ++ ['A'..'Z']

type Env = M.Map Variable Value

type State = (Env, [String], [String])

type Program = [Statement]

data Statement
  = Print  Expr
  | Read   Variable
  | Assign Variable Expr

instance Show Statement where
  show (Read name)        = "read " ++ [name]
  show (Assign name expr) = "assign " ++ [name] ++ show expr
  show (Print expr)       = "print " ++ show expr

-- should distinguish between the types of expressions...
data Expr
  = Op Arith Expr Expr
  | Val Value
  | Var Variable
  | ITE Expr Expr Expr

data Binop = Arith | Cmpr

data Arith
  = Add
  | Sub
  | Mul
  | Div

data Cmpr
  = Gt                          -- >  :: Int -> Int -> Bool
  | Gte                         -- >= :: Int -> Int -> Bool
  | Lt                          -- <  :: Int -> Int -> Bool
  | Lte                         -- <= :: Int -> Int -> Bool
  | Eq                          -- == :: Value -> Value -> Bool
  | Neq                         -- != :: Value -> Value -> Bool

instance Show Expr where
  show (Val val)  = show val
  show (Var name) = [name]
  show (Op Add a b)  = ('(':) . (++ ")") . concat . (intersperse "+") . (map show) $ [a, b]
  show (Op Sub a b)  = ('(':) . (++ ")") . concat . (intersperse "-") . (map show) $ [a, b]
  show (Op Mul a b)  = ('(':) . (++ ")") . concat . (intersperse "*") . (map show) $ [a, b]
  show (Op Div a b)  = ('(':) . (++ ")") . concat . (intersperse "/") . (map show) $ [a, b]
