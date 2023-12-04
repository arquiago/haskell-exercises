module Interpreter where

import Data.Functor.Classes (eq1)
import qualified Data.Map as M
import Foreign.C (e2BIG)
import Parser
import Syntax

-- 1
eval :: Expr -> Maybe Int
eval (Num n) = Just n
eval (Add e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1 + v2)
eval (Sub e1 e2) = do
  x1 <- eval e1
  x2 <- eval e2
  return (x1 - x2)
eval (Mult e1 e2) = do
  x1 <- eval e1
  x2 <- eval e2
  return (x1 * x2)
eval (Div e1 e2) = do
  x1 <- eval e1
  x2 <- eval e2
  if (x2 == 0) then Nothing else return (div x1 x2)

-- 2
evalStr :: String -> Maybe Int
evalStr s = do
  s' <- parseString s
  eval s'

-- 6
type Env = M.Map String Int

evalV :: Env -> Expr -> Maybe Int
evalV env (Num n) = Just n
evalV env (Var n) = M.lookup n env
evalV env (Add e1 e2) = do
  x1 <- evalV env e1
  x2 <- evalV env e2
  return (x1 + x2)
evalV env (Sub e1 e2) = do
  x1 <- evalV env e1
  x2 <- evalV env e2
  return (x1 - x2)
evalV env (Mult e1 e2) = do
  x1 <- evalV env e1
  x2 <- evalV env e2
  return (x1 * x2)
evalV env (Div e1 e2) = do
  x1 <- evalV env e1
  x2 <- evalV env e2
  if x2 == 0 then Nothing else return (div x1 x2)