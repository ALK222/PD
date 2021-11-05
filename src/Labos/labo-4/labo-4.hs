-- ALUMNOS: ALEJANDRO BARRACHINA ARGUDO
--          CARLOS MURCIA MORILLA

-- EJERCICIO 1
type Punto = (Int, Int)

data Direccion = ARRIBA | ABAJO | IZQUIERDA | DERECHA deriving (Eq, Ord, Show)

-- A
mueve :: Punto -> Direccion -> Punto
mueve p m
  | m == ARRIBA = if snd p + 1 > 100 then (fst p, 100) else (fst p, snd p + 1)
  | m == ABAJO = if snd p - 1 < 0 then (fst p, 0) else (fst p, snd p - 1)
  | m == IZQUIERDA = if fst p - 1 < 0 then (0, snd p) else (fst p - 1, snd p)
  | m == DERECHA = if fst p + 1 > 100 then (100, snd p) else (fst p + 1, snd p)
  | otherwise = error "Dirección no valida"

-- B
destino :: Punto -> [Direccion] -> Punto
destino p [] = p
destino p ms = foldl mueve p ms

-- C
trayectoria :: Punto -> [Direccion] -> [Punto]
trayectoria p [] = [p]

-- trayectoria p ms = foldl (\m -> p ++ [mueve p m]) ms

--EJERCICIO 2
data Nat = Cero | Suc Nat deriving (Eq, Ord)

-- A
infix 4 ~+

(~+) :: Nat -> Nat -> Int
a ~+ b = natToInt a + natToInt b

infix 9 ~*

(~*) :: Nat -> Nat -> Int
a ~* b = natToInt a * natToInt b

-- B
natToInt :: Nat -> Int
natToInt Cero = 0
natToInt (Suc n) = 1 + natToInt n

instance Show Nat where show a = show (natToInt a)

-- Ejercicio 3
data Complejo = C Float Float deriving (Eq)

-- instance Show Complejo where show (C a b) = if b > 0 then show a + "+" + show b + "i" else show a + show b + "i"

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

instance Medible (Int, Int) where
  medida a b = case (a, b) of (C x y, C x1 y1) -> sqrt (((x + x1) ^ 2) + ((y + y1) ^ 2))
