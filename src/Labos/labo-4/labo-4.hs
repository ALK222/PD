-- Los warnings de que faltan metodos son muy cansinos
{-# OPTIONS_GHC -Wno-missing-methods #-}

{-|
Module      : Labo4
Description : Laboratorio 4 PD FDI UCM 21-22
License     : GPL-3.0
Maintainer  : Alejandro Barrachina Argudo y Carlos Murcia Morilla
Stability   : experimental
Portability : unknown
-}
module Labo4
where
-- =====================================================
-- * EJERCICIO 1
-- =====================================================

{-|
Representación de un punto de forma (x,y)

-}
data Punto
  = P -- ^ Constructor del punto
    Int -- ^ Coordenada X
    Int -- ^ Coordenada Y

pointSum :: (Ord a, Num a) => a -> a -> a
{-|
= Descripción

Suma un numero a una parte de una coordenada dada. Tiene como rango [0 .. 100]

== Ejemplos

>>> pointSum 100 10
9

>>> pointSum 0 (-1)
100

>>>pointSum 100 1
0
-}
pointSum a b
  | a + b > 100 = (a + b - 1) - 100
  | a + b < 0 = 100 - ( a + b + 1)
  | otherwise = a + b

-- | Representación de una dirección
data Direccion = ARRIBA | ABAJO | IZQUIERDA | DERECHA deriving (
  Eq      -- ^ Derivación estandar de Eq
  , Ord   -- ^Derivación estandar de Ord
  , Show  -- ^ Derivación estandar de Show
  )

-- | Muestra el punto como "(a,b)""
instance Show Punto where show (P a b) = "(" ++ show a ++ "," ++ show b ++ ")"

-- ** A)
mueve :: Punto -> Direccion -> Punto
{-|
= Descripción

Mueve un punto en la dirección dada

== Ejemplos

>>> mueve (P 0 1) IZQUIERDA
(100,1)

>>> mueve (P 0 1) DERECHA
(1,1)

>>> mueve (P 0 1) ARRIBA
(0,2)

>>> mueve (P 0 1) ABAJO
(0,0)
-}
mueve p ARRIBA    = case p of (P a b) -> P a (pointSum b 1)
mueve p ABAJO     = case p of (P a b) -> P a (pointSum b (negate 1))
mueve p DERECHA   = case p of (P a b) -> P (pointSum a 1) b
mueve p IZQUIERDA = case p of (P a b) -> P (pointSum a (negate 1)) b

-- ** B)

destino :: Punto -> [Direccion] -> Punto
{-|
= Descripción

Mueve un punto en las direcciones dadas

== Ejemplos

>>> destino (P 0 0) [ARRIBA, ARRIBA, ABAJO, ABAJO, IZQUIERDA, DERECHA, IZQUIERDA, DERECHA]
(0,0)

>>> destino (P 0 0) [ARRIBA, ARRIBA, ABAJO, IZQUIERDA, IZQUIERDA, DERECHA]
(100,1)
-}
destino p [] = p
destino p ms = foldl mueve p ms

-- ** C)
trayectoria :: Punto -> [Direccion] -> [Punto]
{-|
= Descripción

Mueve un punto en las direcciones dadas mostrando los puntos por los que se han pasado

== Ejemplos

>>> trayectoria (P 0 0) [ARRIBA, ARRIBA, ABAJO, ABAJO, IZQUIERDA, DERECHA, IZQUIERDA, DERECHA]
[(0,0),(0,1),(0,2),(0,1),(0,0),(100,0),(0,0),(100,0),(0,0)]

>>> trayectoria (P 0 0) [ARRIBA, ARRIBA, ABAJO, IZQUIERDA, IZQUIERDA, DERECHA]
[(0,0),(0,1),(0,2),(0,1),(100,1),(99,1),(100,1)]
-}
trayectoria p = foldl (\dir m -> dir ++ [mueve (last dir) m]) [p]

-- =====================================================
-- * EJERCICIO 2
-- =====================================================

-- | Representación de numeros naturales según la aritmética de Peano
data Nat =
  -- | Representación del 0
  Cero
  -- | Representación de cualquier sucesor de 0
  | Suc Nat
  deriving (Eq    -- ^ Derivación estandar de Eq
            , Ord -- ^ Derivación estandar de Ord
            )

-- ** A)
infix 4 ~+

(~+) :: Nat -> Nat -> Nat
{-|
= Descripción

Suma de dos números siguiendo la aritmética de Peano

== Ejemplo

>>>(Suc (Cero)) ~+ (Suc (Suc (Cero)))
3
-}
Cero ~+ x  = x
Suc y ~+ x = y ~+ Suc x

infix 6 ~*
{-|
= Descripción

Multiplicación de dos números siguiendo la aritmética de Peano

== Ejemplo

>>>(Suc (Cero)) ~* (Suc (Suc (Cero)))
2
-}
(~*) :: Nat -> Nat -> Nat
x ~* Cero    = Cero
x ~* (Suc y) = x ~+ (x ~* y)

-- ** B)
natToInt :: Nat -> Int
{-|
= Descripción

Convierte un número representado en aritmética de Peano en un 'Int' normal

== Ejemplo

>>> natToInt (Suc (Suc (Cero)))
2
-}
natToInt Cero    = 0
natToInt (Suc n) = 1 + natToInt n

-- ** C)

-- | Muestra el punto como un número normal
instance Show Nat where show a = show (natToInt a)

-- =====================================================
-- * EJERCICIO 3
-- =====================================================

-- | Representación de un numero complejo
data Complejo = C Float Float deriving (Eq -- ^ Derivación estandar de Eq
                                        )

-- | Representa el número como "a (+-) bi"
instance Show Complejo where show (C a b) = if b > 0 then show a ++ "+" ++ show b ++ "i" else show a ++ show b ++ "i"

-- | Implementación especial para '(+)' '(-)' y '(*)'
instance Num Complejo where
  a + b = case (a, b) of (C x y, C x1 y1) -> C (x + x1) (y + y1)
  a - b = case (a, b) of (C x y, C x1 y1) -> C (x - x1) (y - y1)
  a * b = case (a, b) of (C x y, C x1 y1) -> C ((x * x1) - (y * y1)) ((x * y1) + (y * x1))

-- | Implementación especial para '(/)'
instance Fractional Complejo where
  x / y = case (x, y) of (C a b, C c d) -> C (((a * c) + (b * d)) / ((c * c) + (d * d))) (((b * c) - (a * d)) / ((c * c) + (d * d)))

-- =====================================================
-- * EJERCICIO 4
-- =====================================================

-- | Clase para medir otros tipos de datos
class Medible a where

  medida :: a -> Int -- ^ Coge el dato dado y saca una medida de él

-- | Mediremos True como 0  y false como 1
instance Medible Bool where
  medida True  = 0
  medida False = 1

-- | Suma todas las medidas de la lista
instance (Medible a) => Medible [a] where
  medida []       = 0
  medida (x : xs) = medida x + medida xs

-- | Suma la medida de los dos elementos dados
instance (Medible a, Medible b) => Medible (a, b) where
  medida (x, y) = medida x + medida y
