module StdLogic where

data StdLogic = T | F | X | Z

instance Eq StdLogic where
  T == T = True
  F == F = True
  X == X = True
  Z == Z = True
  _ == _ = False

instance Show StdLogic where
  show T = "T"
  show F = "F"
  show X = "X"
  show Z = "Z"

stdNot :: StdLogic -> StdLogic
stdNot a
  | (==) a T = F
  | (==) a F = T
  | (==) a X = X
  | otherwise = X

stdAnd :: StdLogic -> StdLogic -> StdLogic
stdAnd a b
  | a == F || b == F = F
  | a == X || b == X = X
  | a == T && b == T = T
  | otherwise = X

stdOr :: StdLogic -> StdLogic -> StdLogic
stdOr a b
  | a == T || b == T = T
  | a == F && b == F = F
  | otherwise = X

stdXor :: StdLogic -> StdLogic -> StdLogic
stdXor a b
  | a == T && b == F = T
  | a == F && b == T = T
  | a == F && b == F = F
  | a == T && b == T = F
  | otherwise = X

adder :: StdLogic -> StdLogic -> StdLogic -> (StdLogic, StdLogic)
adder b1 b2 carryIn = (result, carryOut)
  where
    result = stdXor (stdXor b1 b2) carryIn
    temp = stdOr (stdAnd b1 b2) (stdAnd b2 carryIn)
    carryOut = stdOr temp (stdAnd b1 carryIn)

mux :: StdLogic -> StdLogic -> StdLogic -> StdLogic
mux b1 b2 select
  | select == F && (b1 == T || b1 == F) = b1
  | select == T && (b2 == T || b2 == F) = b2
  | otherwise = X
