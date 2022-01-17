{-|
Module      : Labo3
Description : Laboratorio 3 PD FDI UCM 21-22
License     : GPL-3.0
Maintainer  : Alejandro Barrachina Argudo y Carlos Murcia Morilla
Stability   : experimental
Portability : unknown
-}
module Labo3
where
-- =====================================================
-- * EJERCICIO 1
-- =====================================================
-- **  A)
last' :: [a] -> a
{-|
= Descripción

Coge el último elemento de una lista dada

== Ejemplos

>>>last' []
Lista vacia

>>>last [1,2,3,4]
4
-}
last' []       = error "Lista vacia"
last' (x : xs) = foldl (\_ curr -> curr) x xs

-- ** B)
reverse' :: [a] -> [a]
{-|
= Descripción

Hace la lista inversa de una dada

== Ejemplos

>>>reverse' [1,2,3,4]
[4,3,2,1]
-}
reverse' = foldl (\curr next -> next : curr) []

-- ** C)
all' :: (a -> Bool) -> [a] -> Bool
{-|
= Descripción

Devuelve true si todos los elementos de la lista cumplen una condición

== Ejemplos

>>> all' (<10) [1,2,3,4]
True

>>> all' (>10) [1,20,30,40]
False
-}
all' f = foldl (\acc curr -> f curr && acc) True

-- ** D)
min' :: Ord a => [a] -> a
{-|
= Descripción

Devuelve el menor elemento de una lista dada

== Ejemplos

>>> min' []
Lista vacia

>>> min' [2,5,8,2,0]
0
-}
min' []       = error "Lista vacia"
min' (x : xs) = foldl (\minimun curr -> min minimun curr) x xs

-- ** E)
map' :: (a -> b) -> [a] -> [b]
{-|
= Descripción

Genera una lista de elementos resultantes de aplicar un filtro a otra lista de elementos

== Ejemplo
>>> map' (*3) [1,2,3,4]
[3,6,9,12]
-}
map' filtro = foldr (\x xs -> filtro x : xs) []

-- ** F)
filter' :: (a -> Bool) -> [a] -> [a]
{-|
= Descripción

Genera una lista de elementos con los elementos de otra lista que hayan pasado un filtro

== Ejemplo

>>> filter' (>10) [1,20,30,40]
[20,30,40]
-}
filter' filtro = foldr (\x xs -> if filtro x then x : xs else xs) []

-- ** G)
takeWhile' :: (a -> Bool) -> [a] -> [a]
{-|
= Descripción

Coge elementos de la lista dada hasta que la condición falla

== Ejemplos

>>> takeWhile' (<3) [1,2,3,4,5]
[1,2]

>>> takeWhile' (>3) [1,2,3,4,5]
[]
-}
takeWhile' filtro = foldr (\x y -> if filtro x then x : y else []) []

-- =====================================================
-- * EJERCICIO 2
-- =====================================================
-- listaNegPos :: [Integer]
-- listaNegPos = [m | n <- [1 .. 100], m <- [n, negate n]]
listaNegPos :: [Integer]
{-|
= Descripción

Genera una lista alternada de elementos del 1 al 100 de forma [1, -1]

-}
listaNegPos = foldl (\xs x -> xs ++ [x, - x]) [] [1 .. 100]

-- =====================================================
-- * EJERCICIO 3
-- =====================================================
listaParejas' :: (Num b, Enum b, Eq b) => b -> [(b, b)]
{-|
= Descripción

Versión acotada de 'listaParejas'

== Ejemplo

>>> listaParejas' 4
[(0,0),(0,1),(1,0),(0,2),(1,1),(2,0),(0,3),(1,2),(2,1),(3,0),(0,4),(1,3),(2,2),(3,1),(4,0)]
-}
listaParejas' n = [(x, y) | sumaTotal <- [0 .. n], x <- [0 .. sumaTotal], y <- [0 .. sumaTotal], x + y == sumaTotal]

listaParejas :: Integral a => [(a, a)]
{-|
= Descripción

Genera parejas ordenadas que cumplen x + y = z, en orden de z

Ver 'listaParejas'' para ejemplos
-}
listaParejas = [(x, y) | sumaTotal <- [0 ..], x <- [0 .. sumaTotal], y <- [0 .. sumaTotal], x + y == sumaTotal]

-- =====================================================
-- * EJERCICIO 4
-- =====================================================
-- **  A)
sufijos :: [a] -> [[a]]
{-|
= Descripción

Genera todos los sufijos de una lista

== Ejemplo

>>>sufijos [1,2,3,4]
[[1,2,3,4],[2,3,4],[3,4],[4],[]]
-}
sufijos xs = [drop x xs | x <- [0 .. length xs]]

-- ** B)
sublistas :: [a] -> [[a]]
{-|
= Descripción

Genera todas las sublistas de una lista dada

== Ejemplo

>>> sublistas [1,2,3]
[[1],[2],[3],[1,2],[2,3],[1,2,3]]

-}
sublistas [] = []
sublistas xs = [take n (drop i xs) | n <- [1 .. (length xs)], i <- [0 .. length xs - 1], length (drop i xs) >= n]

-- ** C)
permuta :: [a] -> [[a]]
{-|
= Descripción

Genera todas las permutaciones posibles de una lista dada

== Ejemplo

>>> permuta [1,2,3]
[[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
-}
permuta []       = [[]]
permuta (x : xs) = concat [intercalado x ys | ys <- permuta xs]

intercalado :: a -> [a] -> [[a]]
{-|
= Descripción

Devuelve una lista de listas con el elemento dado en cada posición posible de la lista dada

== Ejemplo

>>>intercalado 3 [1,2]
[[3,1,2],[1,3,2],[1,2,3]]
-}
intercalado x []       = [[x]]
intercalado x (y : ys) = (x : y : ys) : [y : zs | zs <- intercalado x ys]

-- ** D)
sumandos :: (Integral a) => a -> [[a]]
{-|
= Descripción

Genera un listado de de listas que al sumar todos sus elementos dan como resultado el numero dado

== Ejemplos

>>> sumandos 3
[[1,1,1],[1,2],[3]]
-}
sumandos n = sumandos' n 0 1

sumandos' :: (Integral a) => a -> a -> a -> [[a]]
{-|
= Descripción

Genera listados de tamaños desde x hasta n que al ser sumados dan como resultado x, siendo i el numero de listados generados

== Ejemplo

>>>sumandos' 4 1 1
[[1,1,1],[1,2],[3]]

>>> sumandos' 4 2 1
[[1,1],[2]]

>>> sumandos' 4 2 2
[[2]]
-}
sumandos' n x i
  | x == n = [[]]
  | otherwise = [z : zs | z <- [y | y <- [1 .. n], y + x <= n, y >= i], zs <- sumandos' n (z + x) z]
