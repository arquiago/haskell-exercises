module Syntax where

import Display

data Expr
  = Num Int
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving (Eq, Show)

-- 3
instance Display Expr where
  display (Num n) = show n
  display (Add e1 e2) = "(" ++ display e1 ++ " + " ++ display e2 ++ ")"
  display (Sub e1 e2) = "(" ++ display e1 ++ " - " ++ display e2 ++ ")"
  display (Mult e1 e2) = "(" ++ display e1 ++ " * " ++ display e2 ++ ")"
  display (Div e1 e2) = "(" ++ display e1 ++ " / " ++ display e2 ++ ")"