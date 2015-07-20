module Ast where
import qualified Data.Map.Strict as M

type Env = M.Map Char Int

type Program = [Command]

data Command = Print Expr
  | Read Char
  | Assign Char Expr
  deriving Show

data Expr = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Val Int
  | Var Char
  deriving Show
