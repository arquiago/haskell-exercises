{-# LANGUAGE TypeSynonymInstances #-}

module VM where

import Display
import Syntax

data Bytecode
  = PUSH Int
  | ADD
  | SUB
  | MULT
  | DIV
  deriving (Eq, Show)

-- 4
compile :: Expr -> [Bytecode]
compile (Num n) = [PUSH n]
compile (Add e1 e2) = compile e1 ++ compile e2 ++ [ADD]
compile (Sub e1 e2) = compile e1 ++ compile e2 ++ [SUB]
compile (Mult e1 e2) = compile e1 ++ compile e2 ++ [MULT]
compile (Div e1 e2) = compile e1 ++ compile e2 ++ [DIV]

-- 5
runBytecode :: [Bytecode] -> Maybe Int
runBytecode bytecode = execute bytecode []

execute :: [Bytecode] -> [Int] -> Maybe Int
execute [] [result] = Just result
execute [] _ = Nothing -- If there's more than one value on the stack, it's an error
execute (PUSH n : rest) stack = execute rest (n : stack)
execute (ADD : rest) (x1 : x2 : stack) = execute rest (x1 + x2 : stack)
execute (SUB : rest) (x1 : x2 : stack) = execute rest (x2 - x1 : stack) -- Note the reversal of operands for subtraction
execute (MULT : rest) (x1 : x2 : stack) = execute rest (x1 * x2 : stack)
execute (DIV : rest) (x1 : x2 : stack)
  | x1 == 0 = Nothing -- Division by zero
  | otherwise = execute rest (x2 `div` x1 : stack)
execute _ _ = Nothing -- Invalid bytecode or stack underflow
