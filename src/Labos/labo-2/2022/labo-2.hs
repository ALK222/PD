{-|
Module      : Labo22022
Description : Laboratorio 2 PD FDI UCM 22-23
License     : GPL-3.0
Maintainer  : Alejandro Barrachina Argudo y Carlos Murcia Morilla
Stability   : experimental
Portability : unknown
-}
module Labo2_2022
where

-- =====================================================
-- * EJERCICIO 1
-- =====================================================

-- ** A)
listaPares :: (Integral a) => a -> [a]
{- |
= Descripción

Genera una lista de números pares hasta el número dado.

== Ejemplo:

>>> listaPares 9
[0,2,4,6,8]
-}
listaPares x = map (*2) [0.. (x `div` 2)]

-- ** B)
listaParesCuadrados :: (Integral a) => a -> [(a,a)]
{- |
= Descripción

Coje un números y devuelve una lista con los cuadrados desde 0 hasta el número en orden inverso y emparejado con su número inicial

== Ejemplos

>>> listaParesCuadrados 3
[(3,9),(2,4),(1,1),(0,0)]
-}
listaParesCuadrados x = let
                           ys = map (^2) [x, x-1 .. 0]
                           xs = [x, x-1 .. 0]
                        in zip xs ys

-- ** C)
listaPotencias :: (Integral a) =>Int -> [a]
{- |
= Descripción

Genera una lista con las potencias de 3 desde 0 hasta el numero dado

== Ejemplo:
>>> listaPotencias 3
[1,3,9]
-}
listaPotencias x = take x (iterate (*3) 1)

-- ** D)
sumaMenores :: Int -> Int
{- |
= Descripción

Sumatorio de los números menores que el dado que sean multiplos de 5 o 3

== Ejemplos

>>> sumaMenores 10
33
-}
sumaMenores x = let xs = filter (\x -> mod x 3 == 0 || mod x 5 == 0) [0..x]
                in sum xs

-- ** E)
primerPrimoMayor :: (Integral a) => a -> a
{- |
= Descripción

calcula el siguiente número primo a uno dado.

Usa 'isPrime'

== Ejemplos

>>> primerPrimoMayor 10
11
-}
primerPrimoMayor x = head $ filter isPrime [x, x+1 ..]

isPrime :: (Integral a) => a -> Bool
{- |
= Descripción

Comprueba si un número es primo

== Ejemplos

>>> isPrime 10
False

>>> isPrime 5
True
-}
isPrime x
  | x < 2 = False
  | otherwise = x == head (filter (\a -> x `mod` a == 0 ) [2 .. x])

-- =====================================================
-- * EJERCICIO 2
-- =====================================================
-- ** A)
iguales :: Eq b => (a -> b) -> (a -> b) -> a -> a -> Bool
{- |
= Descripción

Comprueba si dos funciones son iguales para un ranfo dado de valores

== Ejemplos

>>> iguales primerPrimoMayor sumaMenores 0 10
False

>>> iguales sumaMenores sumaMenores 0 10
True
-}
iguales f g n m = y
  where
    xs = map f [n, m]
    ys = map g [n, m]
    y = all (== True) (zipWith (==) xs ys)

-- ** B)
menorA :: Integral a => a -> a -> (a -> Bool) -> a
{- |
= Descripción

Devuelve el menor numero mayor que n que compla una función dada

== Ejemplos

>>> menorA 22 128 isPrime
23

>>> menorA 14 16 isPrime
Prelude.head: empty list
-}
menorA n m p = y
  where
    y = head (filter p [n .. m])

-- ** C)
menor :: (Num p, Enum p) => p -> (p -> Bool) -> p
{- |
= Descripción

Devuelve el menor numero mayor que n que compla una función dada

== Ejemplos

>>> menor 2077 isPrime
2081
-}
menor n p = y
  where
    y = head (filter p [n ..])

-- ** D)
mayorA :: Enum p => p -> p -> (p -> Bool) -> p
{- |
= Descripción

Mayor número en un intervalo dado que cumple una función dada

== Ejemplos

>>> mayorA 10 20 isPrime
19

>>> mayorA 14 16 isPrime
Prelude.last: empty list
-}
mayorA n m p = y
  where
    y = last (filter p [n .. m])

-- ** E)
pt :: Enum p => p -> p -> (p -> Bool) -> Bool
{- |
= Descripción

Comprueba si todos los elementos de un intervalo verifican una función

== Ejemplos

>>> pt 14 16 isPrime
False

>>> pt 2 3 isPrime
True
-}
pt n m p = y
  where
    y = length (filter p [n .. m]) == length [n .. m]

-- =====================================================
-- * EJERCICIO 3
-- =====================================================
-- ** A)
filter2 :: [a] -> (a -> b) -> (a -> b) -> [[b]]
{- |
= Descripción

Devuelve dos listas con los elementos de una lista dada que cumplen dos funciones

== Ejemplos

>>> filter2 [1..10] odd even
([1,3,5,7,9],[2,4,6,8,10])
-}
filter2 xs f g = [us, vs]
  where
    us = map f xs
    vs = map g xs


-- ** B)
partition :: (a -> Bool) -> [a] -> ([a], [a])
{- |
= Descripción

Parte una lista dada entre elementos que verifican una función y otros que no

== Ejemplos

>>> partition isPrime [0..20]
([2,3,5,7,11,13,17,19],[0,1,4,6,8,9,10,12,14,15,16,18,20])

-}
partition p xs = (us, vs)
  where
    us = filter p xs
    vs = filter (not . p) xs

-- ** C)
mapx :: t -> [t -> b] -> [b]
{- |
= Descripción

Devuelve una lista de valores resultado de aplicar a un valor un listado de funciones. Todos los tipos de las funciones de la lista deben ser iguales

== Ejemplos

>>> mapx 10 [even, odd]
[True,False]

>>> mapx 10 [primerPrimoMayor, sumaMenores]
[11,33]
-}
mapx x = map (\f -> f x)

-- ** D)
filter1 :: [[a]] -> (a -> Bool) -> [[a]]
{- |
= Description

Devuelve una lista con listas de valores que cumplen la propiedad dada

== Ejemplo

>>> filter1 [[1], [1,2,3,4], [2077,2467, 2277]] isPrime
[[],[2,3],[2467]]
-}
filter1 xss p = map (filter p) xss

-- ** E)
filters :: [a] -> [(a -> Bool)] -> [[a]]
{- |
= Descripción

Devuelve una lista con las listas de numeros que cumplen unas funciones dadas en formato de lista

== Ejemplos

>>> filters [1..10] [odd, even, isPrime]
[[1,3,5,7,9],[2,4,6,8,10],[2,3,5,7]]
-}
filters xs ps = map (\p -> filter p xs) ps
