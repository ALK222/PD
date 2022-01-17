{-|
Module      : Labo2
Description : Laboratorio 2 PD FDI UCM 21-22
License     : GPL-3.0
Maintainer  : Alejandro Barrachina Argudo y Carlos Murcia Morilla
Stability   : experimental
Portability : unknown
-}
module Labo2
where
-- =====================================================
-- * EJERCICIO 1
-- =====================================================
-- ** A)
cuadrados :: (Num b, Enum b) => b -> [b]
{- |
= Descripción

Coje un números y devuelve una lista con los cuadrados desde 0 hasta el número

== Ejemplos

>>> cuadrados 3
[0,1,4,9]
-}
cuadrados n = map (\x -> x * x) [0 .. n]

-- ** B)
cuadradoInverso :: (Num a, Enum a) => a -> [(a, a)]
{- |
= Descripción

Coje un números y devuelve una lista con los cuadrados desde 0 hasta el número en orden inverso y emparejado con su número inicial

== Ejemplos

>>> cuadradoInverso 3
[(3,9),(2,4),(1,1),(0,0)]
-}
cuadradoInverso n = map (\x -> (x, x * x)) [n, n -1 .. 0]

-- ** C)
rsumcos :: (Floating a, Enum a) => a -> a
{- |
= Descripción

Sumatorio del coseno de 1 hasta el número dado

== Ejemplos

>>> rsumcos 90
2592.852064773843
-}
rsumcos n = sum (map (\x -> x * abs (cos x)) [1 .. n])

-- ** D)
sumMenores :: Integral a => a -> a
{- |
= Descripción

Sumatorio de los números menores que el dado que sean multiplos de 5 o 3

== Ejemplos

>>> sumMenores 10
33
-}
sumMenores n = sum (filter (\x -> (mod x 3 == 0 || mod x 5 == 0)) [1 .. n])

-- ** E)

siguientePrimo :: Integral a => a -> a
{- |
= Descripción

calcula el siguiente número primo a uno dado.

Usa 'isPrime'

== Ejemplos

>>> siguientePrimo 10
11
-}
siguientePrimo n = head (filter (\x -> isPrime x) [n + 1 ..])

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
-- * EJERCICIO 2
-- =====================================================
-- ** A)
iguales :: Eq b => (a -> b) -> (a -> b) -> a -> a -> Bool
{- |
= Descripción

Comprueba si dos funciones son iguales para un ranfo dado de valores

== Ejemplos

>>> iguales siguientePrimo sumMenores  0 10
False

>>> iguales sumMenores sumMenores  0 10
True
-}
iguales f g n m = y
  where
    xs = map f [n, m]
    ys = map g [n, m]
    y = all (== True) (zipWith (==) xs ys)

-- ** B)
menor :: (Enum a, Num a) => a -> (a -> Bool) -> a
{- |
= Descripción

Devuelve el menor numero mayor que n que compla una función dada

== Ejemplos

>>> menor 10 isPrime
11
-}
menor n p = y
  where
    y = head (filter p [n, n + 1 ..])

-- ** C)
mayorA :: Enum a => a -> a -> (a -> Bool) -> a
{- |
= Descripción

Mayor número en un intervalo dado que cumple una función dada

== Ejemplos

>>> mayorA 10 20 isPrime
19
-}
mayorA n m p = y
  where
    y = last (filter p [n .. m])

-- ** D)
ex :: Enum a => a -> a -> (a -> Bool) -> Bool
{- |
= Descripción

Comprueba si existe algún número en el intervalo dado que verifique una función

== Ejemplos

>>> ex 14 16 isPrime
False

>>> ex 10 15 isPrime
True
-}
ex n m p = y
  where
    y = length (filter p [n .. m]) > 0

-- =====================================================
-- * EJERCICIO 3
-- =====================================================
-- ** A)
filter2 :: [a] -> (a -> Bool) -> (a -> Bool) -> ([a], [a])
{- |
= Descripción

Devuelve dos listas con los elementos de una lista dada que cumplen dos funciones

== Ejemplos

>>> filter2 [1..10] odd even
([1,3,5,7,9],[2,4,6,8,10])
-}
filter2 xs p q = (us, vs)
  where
    us = filter p xs
    vs = filter q xs

-- ** B)
filters :: [a] -> [a -> Bool] -> [[a]]
{- |
= Descripción

Devuelve una lista con las listas de numeros que cumplen unas funciones dadas en formato de lista

== Ejemplos

>>> filters [1..10] [odd, even, isPrime]
[[1,3,5,7,9],[2,4,6,8,10],[2,3,5,7]]
-}
filters xs ys = map (\f -> filter f xs) ys

-- ** C)
mapx :: t -> [t -> b] -> [b]
{- |
= Descripción

Devuelve una lista de valores resultado de aplicar a un valor un listado de funciones. Todos los tipos de las funciones de la lista deben ser iguales

== Ejemplos

>>> mapx 10 [even, odd]
[True,False]

>>> mapx 10 [siguientePrimo, sumMenores]
[11,33]
-}
mapx x fs = map (\f -> f x) fs
