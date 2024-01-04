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
-- | Time for the exercise
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
{-|
=Description

Gets the number of digits of a given value

==Example
>>> digitos 1
1

>>> digitos 123912891289
12
-}
digitos 0 = 0
digitos x = digitos (x `div` 10) + 1

-- ** B)
sumaDigitos :: Int -> Int
{-|
=Description

Adds every single digit of a given number

==Example

>>> sumaDigitos 1
1

>>> sumaDigitos 18982918192283429138194213189
82
-}
sumaDigitos x
    | x < 10 = x
    | otherwise = x `mod` 10 + sumaDigitos (x `div` 10)

reduccion :: Int -> Int
{-|
=Description

Adds digits ('sumaDigitos') from a number until de result is one single digit. Single digits bellow 0 are converted to positive values

==Example
>>> reduccion (-1)
1

>>> reduccion 12893213913819238192312819
2
-}
reduccion x
    | x < 0 = reduccion (-x)
    | x < 10 = x
    | otherwise = let y = sumaDigitos x in if y > 10 then reduccion y else y

-- ** C)
perm :: Integer -> Integer
{-|
=Description

Permutations of a given number

==Example
>>> perm 8
40320
-}
perm 0 = 1
perm x = perm (x - 1) * x

-- ** D)
var :: Integer -> Integer -> Integer
{-|
=Description

Variations of two given numbers

==Example
>>> var 2 1
2

>>> var 123 5
25928567280
-}
var n m = perm n `div` perm (n - m)

-- ** E)
comb :: Integer -> Integer -> Integer
{-|
=Description

Combinations of two given numbers

==Examples

>>>comb 2 1
2

>>> comb 12425 12423
77184100
-}
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

-- =====================================================
-- * EJERCICIO 6
-- =====================================================

y :: Bool -> Bool -> Bool
-- | Conjunction operator
y True True = True
y a b       = False

o :: Bool -> Bool -> Bool
-- | Disjunction operator
o False False = False
o a b         = True

no :: Bool -> Bool
-- | Not operator
no True  = False
no False = True

implica :: Bool -> Bool -> Bool
-- | Implication operator
implica True False = False
implica a b        = True

eq :: Bool -> Bool -> Bool
-- | Equivalence operator
eq False False = True
eq True True   = True
eq a b         = False

oEx :: Bool -> Bool -> Bool
-- | Exclusive disjunction operator
oEx True False = True
oEx False True = True
oEx a b        = False

infixr 3 &&&
(&&&) :: Bool -> Bool -> Bool
-- | Conjunction operator
True &&& True = True
a &&& b       = False

infix 3 |||
(|||) :: Bool -> Bool -> Bool
-- | Disjunction operator
False ||| False = False
a ||| b         = True

infix 3 -->
(-->) :: Bool -> Bool -> Bool
-- | Implication operator
True --> False = False
a --> b        = True

infix 3 ===
(===) :: Bool -> Bool -> Bool
-- | Equivalence operator
False === False = True
True === True   = True
a === b         = False

infix 3 ||=
(||=) :: Bool -> Bool -> Bool
-- | Exclusive disjunction operator
True ||= False = True
False ||= True = True
a ||= b        = False


-- =====================================================
-- * EJERCICIO 8
-- =====================================================
f8 :: Int -> Int -> Int -> Int
{-|
=Description

La función toma tres argumentos y cumple con las siguientes condiciones:

  (i) Ser estricta en el primer argumento.

  (ii) No ser estricta ni en el segundo ni en el tercer argumento.

  (iii) Ser conjuntamente estricta en el segundo y tercer argumento.

==Examples
>>> f8 1 undefined undefined
1

>>> f8 0 1 2
3

>>> f8 0 1 undefined
Prelude.undefined
-}
f8 0 y z = y + z
f8 x _ _ = x

-- =====================================================
-- * EJERCICIO 9
-- =====================================================

{- |
=Description

Arithmetic mean of a list of numbers

==Example

>>> media [1,2,3,4]
2.5
-}
media xs =
  let s = sum xs
      l = length xs
   in fromIntegral s / fromIntegral l


-- =====================================================
-- * EJERCICIO 10
-- =====================================================
last' :: [a] -> a
{-|
=Description

Returns the last element of a given list

==Example
>>> last' [1,2,3,4]
4
-}
last' [x]    = x
last' (x:xs) = last xs

init' :: [a] -> [a]
{-|
=Description

Returns all element except the las one of a list

==Examples
>>> init [1,2,3,4]
[1,2,3]
-}
init' [x]    = []
init' (x:xs) = x : init' xs

initLast :: [a] -> ([a], a)
{-|
=Description

Returns a tuple with the 'init'' of a list and the 'last'' of a list

==Example
>>> initLast [1,2,3,4]
([1,2,3],4)
-}
initLast xs = (init' xs, last' xs)

concat' :: [[a]] -> [a]
{-|
=Description

Concatenates all given lists into one

==Example
>>> concat' [[1],[2,3],[4],[5]]
[1,2,3,4,5]
-}
concat' []           = []
concat' [x:xs]       = x:xs
concat'([]:xss)      = concat xss
concat' ((x:xs):xss) = x:concat' (xs:xss)

auxList :: Int -> Int -> Int -> [a] -> [a]
{-|
=Description

Returns the elements of a list between two given indexes

==Examples

>>> auxList 0 0 2 [1,2,3,4,5]
[1,2,3]
-}
auxList _ _ _ [x] = [x]
auxList curr ini fin (x:xs)
  | curr >=ini && curr < fin = x : auxList (curr + 1) ini fin xs
  | fin == curr = [x]
  | otherwise = auxList (curr + 1) ini fin xs

take' :: Int -> [a] -> [a]
{-|
=Description

Takes the n first elements of a list

==Example
>>> take' 2 [1,2,3,4]
[1,2]
-}
take' n = auxList 0 0 (n-1)

drop' :: Int -> [a] -> [a]
{-|
=Description

Erases the n first elements of a list

==Example
>>> drop' 2 [1,2,3,4]
[3,4]
-}
drop' n xs = auxList 0 n (length xs) xs

splitAt' :: Int -> [a] -> ([a],[a])
{-|
=Description

Splits a list in the given index

==Example
>>> splitAt' 2 [1,2,3,4]
([1,2],[3,4])
-}
splitAt' n xs = (take' n xs, drop' n xs)

nub :: (Eq a) => [a] -> [a]
{-|
=Description

Returns a list without the repeated values of the given list

==Example
>>> nub [1,2,3,4,1,2,3,4,3,2,6,4,2,0]
[1,2,3,4,6,0]
-}
nub = foldl (\new x -> if x `notElem` new then new ++ [x] else new) []

quicksort :: Ord a => [a] -> [a]
{-|
=Description
Quicksort implementation

==Examples
>>> quicksort [4,7,3,6,1,89,52,76,13,0,23]
[0,1,3,4,6,7,13,23,52,76,89]
-}
quicksort []     = []
quicksort (x:xs) = quicksort smaller ++ [x] ++ quicksort larger
  where
    smaller = filter (<= x) xs
    larger  = filter (> x) xs

and' :: [Bool] -> Bool
{-|
=Description

Conjunction of al elements of a list

==Example
>>> and' [True,False, True ]
False
-}
and' = foldl (&&) True -- all (== True)

or' :: [Bool] -> Bool
{-|
=Description

Disjunction of al elements of a list

==Example
>>> or' [True,False, True ]
True
-}
or' = foldl (||) True -- any (== True)

sum' :: [Int] -> Int
{-|
=Description

Sum of al the numbers in the list

==Example
>>> sum' [1..100]
5050
-}

sum' = foldr (+) 0

product' :: [Integer] -> Integer
{-|
=Description

Product of al the numbers in the list

==Example
>>> product' [2..5]
120
-}

product' = foldl (*) 1

lmedia :: [[[a]]] -> Float
{-|
=Description

Mean of sizes of al elements in a list of lists
-}
lmedia xss = fromIntegral n / fromIntegral m where
  xs = concat xss
  n = foldl (\t x-> t+ length x) 0 xs
  m = length xs

-- =====================================================
-- * EJERCICIO 11
-- =====================================================
-- ** A)
reverse' :: [a] -> [a]
{-|
=Description
Reverse a given list in o(n^2) time

==Example

>>> reverse' [1,2,3,4]
[4,3,2,1]
-}
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

-- ** B)
reverse'' :: [a] -> [a]
{-|
=Description
Reverse a given list in o(n) time

==Example

>>> reverse'' [1,2,3,4]
[4,3,2,1]
-}
reverse'' xs = reverseAux xs [] where
  reverseAux [] acc     = acc
  reverseAux (x:xs) acc = reverseAux xs (x:acc)

