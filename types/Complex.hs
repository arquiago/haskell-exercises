
import Epsilon

data Complex
  = Rect Double Double
  | Polar Double Double

isRect :: Complex -> Bool
isRect (Rect _ _) = True
isRect _ = False

isPolar :: Complex -> Bool
isPolar (Polar _ _) = True
isPolar _ = False

toDegree :: Double -> Double
toDegree a = (180 / pi) * a

takeDegree :: Complex -> Double
takeDegree (Polar _ b) = b
takeDegree z@(Rect a b) = takeDegree (toPolar z)

takeMag :: Complex -> Double
takeMag (Polar a _) = a
takeMag z@(Rect a b) = takeMag (toPolar z)

takeReal :: Complex -> Double
takeReal (Rect a _) = a
takeReal z@(Polar a b) = takeReal z

takeImg :: Complex -> Double
takeImg (Rect _ b) = b
takeImg z@(Polar a b) = takeImg z

toPolar :: Complex -> Complex
toPolar (Rect a b) = Polar r t
  where
    r = sqrt (a ^ 2 + b ^ 2)
    t = toDegree (atan2 b a)
toPolar (Polar a b) = Polar a b

toRect :: Complex -> Complex
toRect (Polar a b) = Rect x y
  where
    x = a * cos b
    y = a * sin b
toRect c = c

-- As funções abaixo permitem que comparemos
-- membros do tipo Complex.
instance Eq Complex where
  (Rect x y) == (Rect x' y') = x ≈ x' && y ≈ y'
  p1@(Polar _ _) == p2@(Polar _ _) = toRect p1 == toRect p2
  c@(Rect _ _) == p@(Polar _ _) = c == toRect p
  p@(Polar _ _) == c@(Rect _ _) = c == toRect p

showSign :: Double -> String
showSign x
  | x >= 0 = "+"
  | x < 0 = "-"
showSign _ = ""

-- Defina as operações da typeclass Num
-- para o tipo Complex.
--
--
funcaoAdd ::(Num a) => a -> a -> a 
funcaoAdd a b = a + b 

instance Num Complex where
  (+) (Rect a b) (Rect c d) = Rect (a + c) (c + d)
  (+) (Polar a b) (Polar c d) = result
    where
      op1 = toRect (Polar a b)
      op2 = toRect (Polar c d)
      result = toPolar ((+) op1 op2)
  (+) (Rect a b) (Polar c d) = Rect (a + c) (b + d)
  (+) (Polar a b) (Rect c d) = Rect (a + c) (b + d)

  (-) (Rect a b) (Rect c d) = Rect (a - c) (b - d)
  (-) z@(Polar a b) z2@(Rect c d) = z' - z2
    where
      z' = toRect z
  (-) z@(Rect a b) z2@(Polar c d) = z - z2'
    where
      z2' = toRect z2
  (-) z@(Polar a b) z2@(Polar c d) = z' - z2'
    where
      z' = toRect z
      z2' = toRect z2

  (*) (Rect a b) (Rect c d) = op1
    where
      op1 = toPolar (Rect (a * c - b * d) (a * d + b * c))
  (*) z1@(Polar a b) z2@(Polar c d) = Polar x y
    where
      x = a * c
      y = b + d
  (*) z1@(Polar a b) z2@(Rect c d) = Polar x y
    where
      z2' = toPolar z2
      y = b + takeDegree z2'
      x = a * takeMag z2'
  (*) z1@(Rect a b) z2@(Polar c d) = Polar x y
    where
      z1' = toPolar z1
      y = d + takeDegree z1'
      x = c * takeMag z1'

  abs (Polar a b) = Polar a 0
  abs z@(Rect a b) = Polar x 0
    where
      x = takeMag (toPolar z)
  signum z@(Rect a b) = Polar 1 x
    where
      x = takeDegree (toPolar z)
  signum (Polar a b) = Polar 1 b
  fromInteger n = Rect (fromInteger n) 0
  negate (Rect a b) = Rect a (- b)
  negate z@(Polar a b) = Rect x (- y)
    where
      z' = toRect z
      x = takeReal z'
      y = takeImg z'

-- Defina a função show para um parâmetro Complex.
-- Essa função converte um elemento para uma string.
instance Show Complex where
  show (Rect a b)
    | b > 0 || b < 0 = show a ++ " " ++ showSign b ++ " " ++ show (abs b) ++ "i"
    | b == 0 = show a
  show (Polar a b) = show a ++ "#" ++ show b
