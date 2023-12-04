module Data.Functions where

import Prelude hiding (sequence)

-- Parte 1

member :: Eq a => a -> [a] -> Bool
member x lx = foldr (\e ac -> e == x || ac) False lx

count :: (Num a, Eq b) => b -> [b] -> a
count x xl = foldr (\y ac -> if x == y then ac + 1 else ac) 0 xl

forall :: (a -> Bool) -> [a] -> Bool
forall p xa = foldr (\e ac -> p e && ac) True xa

exists :: (a -> Bool) -> [a] -> Bool
exists p xa = foldr (\e ac -> p e || ac) False xa

first :: (a -> Bool) -> [a] -> Maybe a
first p xa = foldr (\e ac -> if p e then Just e else ac) Nothing xa

isOne 1 = True
isOne _ = False

single :: (a -> Bool) -> [a] -> Bool
single p xa = isOne (foldr (\y ac -> if p y then ac + 1 else ac) 0 xa)

mostly :: (a -> Bool) -> [a] -> Bool
mostly p xs = (\b -> b > (length xs) `div` 2) (foldr (\y ac -> if p y then ac + 1 else ac) 0 xs)

mostlyTrue :: [Bool] -> Bool
mostlyTrue xs = (\b -> b > (length xs) `div` 2) (foldr (\y ac -> if y then ac + 1 else ac) 0 xs)

-- Parte 2
process :: (Eq a) => a -> (a, Int) -> (a, Int)
process x (v, 0) = (x, 1)
process x (v, a) = if x == v then (v, a + 1) else (v, a -1)

majority :: Eq a => [a] -> Maybe a
majority xs = if actual_majority c xs then Just c else Nothing
  where
    c = fst (foldr process (head xs, 1) (tail xs))

actual_majority e es = mostly (e ==) es

{-majority [] = Nothing
majority xs = Just (fst (foldl process (head xs, 1) (tail xs)))
-}
collatz1 :: Int -> Int
collatz1 1 = 1
collatz1 n = if even n then div n 2 else n * 3 + 1

collatz :: Int -> [Int]
collatz 1 = [1]
collatz n = n : collatz (collatz1 n)

isFixpoint :: Eq a => (a -> a) -> a -> Bool
isFixpoint f x = f x == x

findFixpoint :: Eq a => (a -> a) -> a -> Int -> Maybe a
findFixpoint f x 0 = Nothing
findFixpoint f x limit =
  if f x == x
    then Just x
    else findFixpoint f (f x) (limit -1)

testCollatzConjecture :: Int -> [Int] -> [Bool]
testCollatzConjecture x (y : ys) = ((findFixpoint collatz1 y x) /= Nothing) : testCollatzConjecture x ys
testCollatzConjecture x [] = [True]

tooBig :: Double -> Bool
tooBig d = d > 0.0001

nearlyEqual :: Double -> Double -> Bool
nearlyEqual 0 0 = True
nearlyEqual x y = r >= 0.9999 && r <= 1.0001
  where
    r = x / y

sequence :: (Int -> Double) -> Int -> [Double]
sequence f i = map f [i ..]

series :: (Int -> Double) -> Int -> [Double]
series f i = tail (scanl (\ac e -> f e + ac) (f i) [i ..])

computeLn2 :: Double
computeLn2 = ln2 1

ln2 :: Int -> Double
ln2 1 = 1 + ln2 2
ln2 n = term + nterm
  where
    term = ((-1) ^ (n + 1)) / fromIntegral n
    nterm = if abs term < 1e-4 then 0 else ln2 (n + 1)

computeE = auxe 0

auxe 0 = 1 + auxe 1
auxe n = term + nterm
  where
    term = 1 / product [1 .. n]
    nterm = if abs term < 1e-4 then 0 else auxe (n + 1)

fib = fst . fib2

fib2 0 = (1, 1)
fib2 1 = (1, 2)
fib2 n
  | even n = (a * a + b * b, c * c - a * a)
  | otherwise = (c * c - a * a, b * b + c * c)
  where
    (a, b) = fib2 (n `div` 2 - 1)
    c = a + b

computePsi :: Double
computePsi = psi 0

fibrecip n = 1 / fib n

psi :: Int -> Double
psi n = recip + nterm
  where
    recip = fibrecip n
    nterm = if recip < 1e-9 then 0 else psi (n + 1)
