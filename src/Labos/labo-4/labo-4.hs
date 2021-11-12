-- Los warnings de que faltan metodos son muy cansinos
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- ALUMNOS: ALEJANDRO BARRACHINA ARGUDO
--          CARLOS MURCIA MORILLA

-- EJERCICIO 1
data Punto = P Int Int

pointSum :: (Ord a, Num a) => a -> a -> a
-- Adds a number to a given coordinate in a range of [0..100]
pointSum a b
  | a + b > 100 = (a + b - 1) - 100
  | a + b < 0 = negate (a + b)
  | otherwise = a + b

data Direccion = ARRIBA | ABAJO | IZQUIERDA | DERECHA deriving (Eq, Ord, Show)

instance Show Punto where show (P a b) = "(" ++ show a ++ "," ++ show b ++ ")"

-- A
mueve :: Punto -> Direccion -> Punto

-- | Moves a point in a given direction
mueve p ARRIBA = case p of (P a b) -> P a (pointSum b 1)
mueve p ABAJO = case p of (P a b) -> P a (pointSum b (negate 1))
mueve p DERECHA = case p of (P a b) -> P (pointSum a 1) b
mueve p IZQUIERDA = case p of (P a b) -> P (pointSum a (negate 1)) b

-- B

destino :: Punto -> [Direccion] -> Punto

-- | moves a point across a list of directions
destino p [] = p
destino p ms = foldl mueve p ms

-- C
trayectoria :: Punto -> [Direccion] -> [Punto]

-- | Shows a list of points where a point has been moving given a direcction list
trayectoria p = foldl (\dir m -> dir ++ [mueve (last dir) m]) [p]

--EJERCICIO 2
data Nat = Cero | Suc Nat deriving (Eq, Ord)

-- A
infix 4 ~+

(~+) :: Nat -> Nat -> Nat
Cero ~+ x = x
Suc y ~+ x = y ~+ Suc x

infix 6 ~*

(~*) :: Nat -> Nat -> Nat
x ~* Cero = Cero
x ~* (Suc y) = x ~+ (x ~* y)

-- B
natToInt :: Nat -> Int
natToInt Cero = 0
natToInt (Suc n) = 1 + natToInt n

instance Show Nat where show a = show (natToInt a)

-- Ejercicio 3
data Complejo = C Float Float deriving (Eq)

instance Show Complejo where show (C a b) = if b > 0 then show a ++ "+" ++ show b ++ "i" else show a ++ show b ++ "i"

instance Num Complejo where
  a + b = case (a, b) of (C x y, C x1 y1) -> C (x + x1) (y + y1)
  a - b = case (a, b) of (C x y, C x1 y1) -> C (x - x1) (y - y1)
  a * b = case (a, b) of (C x y, C x1 y1) -> C ((x * x1) - (y * y1)) ((x * y1) + (y * x1))

instance Fractional Complejo where
  x / y = case (x, y) of (C a b, C c d) -> C (((a * c) + (b * d)) / ((c * c) + (d * d))) (((b * c) - (a * d)) / ((c * c) + (d * d)))

-- EJERCICIO 4

class Medible a where
  medida :: a -> Int

instance Medible Bool where
  medida True = 0
  medida False = 1

instance (Medible a) => Medible [a] where
  medida [] = 0
  medida (x : xs) = medida x + medida xs

instance (Medible a, Medible b) => Medible (a, b) where
  medida (x, y) = medida x + medida y
