{-|
Module      : Lote1
Description : Lote de ejercicios 1 PD FDI UCM 23-24
License     : GPL-3.0
Maintainer  : Alejandro Barrachina Argudo
Stability   : experimental
Portability : unknown
-}
module Lote1
where

-- =====================================================
-- * EJERCICIO 1
-- =====================================================

-- ** A)
petaFlop :: Integer
-- | Flops in a petaFlop
petaFlop = 10 ^ 15

fornl :: Integer
-- | Computing power of the best computer of Frontier
fornl =  1194 * petaFlop

yearsToSec :: Integer
-- | Seconds in a year
yearsToSec = 365 * 24 * 3600

frontierInteger :: Integer
-- | Power of operation of the Frontier since the beginning of time
-- | >>> frontierInteger
-- 515859580800000000000000000000000000
frontierInteger = (yearsToSec * 13700 *(10 ^ 6)) * fornl


petaFlop' :: Int
-- | Flops in a petaFlop
petaFlop' = 10 ^ 15

fornl' :: Int
-- | Computing power of the best computer of Frontier
fornl' =  1194 * petaFlop'

yearsToSec' :: Int
-- | Seconds in a year
yearsToSec' = 365 * 24 * 3600

frontierInt :: Int
-- | Power of operation of the Frontier since the beginning of time
-- | >>> frontierInt
-- -7928755680649936896
frontierInt = (yearsToSec' * 13700 * (10 ^6)) * fornl'


petaFlop'' :: Double
-- | Flops in a petaFlop
petaFlop'' = 10 ^ 15

fornl'' :: Double
-- | Computing power of the best computer of Frontier
fornl'' =  1194 * petaFlop''

yearsToSec'' :: Double
-- | Seconds in a year
yearsToSec'' = 365 * 24 * 3600

frontierDouble :: Double
-- | Power of operation of the Frontier since the beginning of time
-- | >>> frontierDouble
-- 5.158595808e35
frontierDouble = (yearsToSec'' * 13700 *(10 ^6)) * fornl''


-- ** B)
time :: Integer
time = 10 ^10

totalYears :: Integer
-- | >>> totalYears
-- 317
totalYears = div time yearsToSec

-- ** C)

daysToSec :: Integer
-- | Seconds in a day
daysToSec = 24 * 3600

hoursToSec :: Integer
-- | Seconds in an hour
hoursToSec = 3600

minsToSec :: Integer
-- | Segundos totales en un minuto
minsToSec = 60

years :: Integer
-- | Caclulo de los años equivalentes a 'time'
years = div time yearsToSec

days :: Integer
-- | Calculo de las horas equivalentes a 'time' quitando 'years'
days = div (time - (years * yearsToSec)) daysToSec

hours :: Integer
-- | Calculo de las horas equivalentes a 'time' quitando 'days' y 'years'
hours = div (time - (years * yearsToSec) - (days * daysToSec)) hoursToSec

mins :: Integer
-- | Calculo de las horas equivalentes a 'time' quitando 'days', 'years' y 'hours'
mins = div (time - (years * yearsToSec) - (days * daysToSec) - (hours * hoursToSec)) minsToSec

segs :: Integer
-- | Calculo de las horas equivalentes a 'time' quitando 'days', 'years', 'hours' y 'mins'
segs = time - (years * yearsToSec) - (days * daysToSec) - (hours * hoursToSec) - (mins * minsToSec)

solc :: (Integer, Integer, Integer, Integer, Integer)
-- | Tupla con el tiempo total de 'time' en años, dias, horas, minutos y segundos
solc = (years, days, hours, mins, segs)

-- ** D)

sold :: Integer -> (Integer, Integer, Integer, Integer, Integer)
{- |
= Descripción

Coge el tiempo dado y lo convierte a (Años, Días, Horas, Minutos, Segundos)

== Ejemplos

>>> sold 1000000
(0,11,13,46,40)
-}
sold x =
  let years = div x yearsToSec
      a = mod x yearsToSec
  in
      let days = div a daysToSec
          b = mod a daysToSec
      in
        let hours = div b hoursToSec
            c = mod b hoursToSec
  in (years, days, hours, div c minsToSec, mod c minsToSec)

-- =====================================================
-- * EJERCICIO 2
-- =====================================================

f :: Int -> Int -> Int
{- |

= Descripción

Función f usando notación infija

== Ejemplos

>>> f 7 4
-14

-}
f x y = 2*x - y*x

f' :: Int -> Int -> Int
{- |

= Descripción

Función f usando notación prefija

== Ejemplo

>>> f' 7 4
-14
-}
f' x y = (-) ((*) 2 x) ((*)y x)

g :: Int -> Int
{- |

= Descripción

Función g

== Ejemplo

>>> g (-2)
32
-}
g x = f (f 2 x) (f x 1)

h :: Int -> Int -> Int -> Int
{- |

= Descripción

Función h usando notación infija

== Ejemplo

>>> h 1 2 3
0

-}
h x y z = f (f (x + 2*y) (g 3)) (5 - (g z) - y)

h' :: Int -> Int -> Int -> Int
{- |

= Descripción

Función h usando notación posfija

== Ejemplo

>>> h' 1 2 3
0

-}
h' x y z = f (f (x + ((*)2 y)) (g 3)) (((-) 5 ((-) (g z) y)))

i :: Int -> Int -> Int
{- |

= Descripción

Función i usando notación guardas

== Ejemplo

>>> i 1 2
0

-}
i x y
    | x >= y && y > 0 = x - y
    | x > 0 && y > x = 0
    |otherwise = y - x

i' :: Int -> Int -> Int
{- |

= Descripción

Función i usando notación if _ then _ else

== Ejemplo

>>> i 1 2
0

-}
i' x y = if x >= y && y > 0 then
    x-y
    else if x > 0 && y > x then
        0 else y - x

-- =====================================================
-- * EJERCICIO 3
-- =====================================================

-- ** A)

digitos :: Int -> Int
digitos 0 = 0
digitos x = digitos (x `div` 10) + 1

-- ** B)
sumaDigitos :: Int -> Int
sumaDigitos x
    | x < 10 = x
    | otherwise = x `mod` 10 + sumaDigitos (x `div` 10)

reduccion :: Int -> Int
reduccion x
    | x < 0 = reduccion (-x)
    | x < 10 = x
    | otherwise = let y = sumaDigitos x in if y > 10 then reduccion y else y

-- ** C)
perm :: Int -> Int
perm 0 = 1
perm x = perm (x - 1) * x

-- ** D)
var :: Int -> Int -> Int
var n m = perm n `div` perm (n - m)

-- ** E)
comb :: Int -> Int -> Int
comb n m = perm n `div` (perm m * perm (n - m))

-- =====================================================
-- * EJERCICIO 4
-- =====================================================

factores :: Integral a => a -> [a]
{- |
= Descripción

Calcula todos los divisores de un número dado

== Ejemplos

>>> factores 10
[1,2,5,10]
-}
factores n = [x | x <- [1 .. n], mod n x == 0]

isPrime :: Integral a => a -> Bool
{- |
= Descripción

Comprueba si un número es primo

Usa 'factores'

== Ejemplos

>>> isPrime 10
False

>>> isPrime 5
True
-}
isPrime n = factores n == [1, n]

-- =====================================================
-- * EJERCICIO 5
-- =====================================================

fibonacci :: Int -> Integer
{- |
= Descipción

Funcion de fibonacci de aridad 1

== Ejemplos

>>> fibonacci 2
2

>>> fibonacci 100
573147844013817084101

-}
fibonacci n = fibs !! n
  where
    fibs = 1 : 1 : zipWith (+) fibs (tail fibs) -- Vamos sumando el final de la cola al ultimo elemento
