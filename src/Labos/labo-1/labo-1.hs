{-|
Module      : Labo1
Description : Laboratorio 1 PD FDI UCM 21-22
License     : GPL-3.0
Maintainer  : Alejandro Barrachina Argudo y Carlos Murcia Morilla
Stability   : experimental
Portability : unknown
-}
module Labo1
where

-- =====================================================
-- * EJERCICIO 1
-- =====================================================
-- ** A)
time :: Integer
-- | tiempo base del ejercicio 1
time = 10 ^ 6

yearsSeg :: Integer
-- | Segundos totales en un año
yearsSeg = 365 * 24 * 3600

daysSeg :: Integer
-- | Segundos totales en un día
daysSeg = 24 * 3600

hoursSeg :: Integer
-- | Segundos totales en una hora
hoursSeg = 3600

minsSeg :: Integer
-- | Segundos totales en un minuto
minsSeg = 60

years :: Integer
-- | Caclulo de los años equivalentes a 'time'
years = div time yearsSeg

days :: Integer
-- | Calculo de las horas equivalentes a 'time' quitando 'years'
days = div (time - (years * yearsSeg)) daysSeg

hours :: Integer
-- | Calculo de las horas equivalentes a 'time' quitando 'days' y 'years'
hours = div (time - (years * yearsSeg) - (days * daysSeg)) hoursSeg

mins :: Integer
-- | Calculo de las horas equivalentes a 'time' quitando 'days', 'years' y 'hours'
mins = div (time - (years * yearsSeg) - (days * daysSeg) - (hours * hoursSeg)) minsSeg

segs :: Integer
-- | Calculo de las horas equivalentes a 'time' quitando 'days', 'years', 'hours' y 'mins'
segs = time - (years * yearsSeg) - (days * daysSeg) - (hours * hoursSeg) - (mins * minsSeg)

sol1a :: (Integer, Integer, Integer, Integer, Integer)
-- | Tupla con el tiempo total de 'time' en años, dias, horas, minutos y segundos
sol1a = (years, days, hours, mins, segs)

-- ** B)

sol1b :: Integer -> (Integer, Integer, Integer, Integer, Integer)
{- |
= Descripción

Coge el tiempo dado y lo convierte a (Años, Días, Horas, Minutos, Segundos)

== Ejemplos

>>> sol1b 1000000
(0,11,13,46,40)
-}
sol1b x =
  let years = div x yearsSeg
      a = mod x yearsSeg
  in
      let days = div a daysSeg
          b = mod a daysSeg
      in
        let hours = div b hoursSeg
            c = mod b hoursSeg
  in (years, days, hours, div c minsSeg, mod c minsSeg)

-- =====================================================
-- * EJERCICIO 2
-- =====================================================
-- ** A)
sol2A :: Integer -> Bool
{- |
= Descripción

Coje el año dado y devuelve True si es bisiesto, False en caso contrario

== Ejemplos
>>> sol2A 2008
True

>>> sol2A 2022
False
-}
sol2A year =
  if ((mod year 4 == 0) && (mod year 100 /= 0)) || (mod year 400 == 0) then True else False

-- ** B)
sol2B :: Integer -> Bool
{- |
= Descripción

Coje el año dado y devuelve True si es bisiesto, False en caso contrario

== Ejemplos
>>> sol2A 2008
True

>>> sol2A 2022
False
-}
sol2B year
  | mod year 400 == 0 = True
  | (mod year 4 == 0) && (mod year 100 /= 0) = True
  | otherwise = False

-- =====================================================
-- * EJERCICIO 3
-- =====================================================

{- |
= Descripción

Error de tipado ya que la division / no trabaja con Integer

-}
{-|
= Descripción

Calcula la media de un listado de numeros dado

>>> sol3 [1,2,3,4]
2.5

-}
sol3 xs =
  let s = sum xs
      l = length xs
  in fromIntegral s / fromIntegral l

-- =====================================================
-- * EJERCICIO 4
-- =====================================================
calculoDigitos :: Integer -> Integer
{- |
= Descripción

Cálculo del número de dijitos de un numero dado

== Ejemplos
>>> calculoDigitos 0
1

>>> calculoDigitos 1984
4
-}
calculoDigitos x
  | x < 10 = 1
  | otherwise = 1 + calculoDigitos (div x 10)

sumaDigitos :: Integral t => t -> t
{- |
= Descripción

Suma todos los digitos de un numero dado

== Ejemplos
>>> sumaDigitos 0
0

>>> sumaDigitos 1984
22
-}
sumaDigitos x
  | x < 10 = x
  | otherwise = mod x 10 + sumaDigitos (div x 10)

reduccion :: Integral t => t -> t
{- |
= Descripción

Suma todos los digitos de un numero dado hasta que el total es menor que 10

== Ejemplos
>>> reduccion 0
0

>>> reduccion 1984
4
-}
reduccion x
  | x < 0 = reduccion (-x)
  | x < 10 = x
  | otherwise = let y = sumaDigitos x in if y < 10 then y else reduccion y

-- =====================================================
-- * EJERCICIO 5
-- =====================================================

bools :: Bool -> Bool -> Bool
-- | Disyunción booleana definida por ajuste de patrones
bools x False = x
bools False x = x
bools x y     = x || y
