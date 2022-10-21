{-|
Module      : Labo3
Description : Laboratorio 3 PD FDI UCM 22-23
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
any' :: (a -> Bool) -> [a] -> Bool
{-|
= Descripción

Devuelve true si todos los elementos de la lista cumplen una condición

== Ejemplos

>>> any' (<10) [1,2,3,4]
True

>>> any' (<10) [10, 20, 30, 5]
True

>>> any' (>10) [1,2, 3, 4]
False

-}
any' f = foldl (\acc curr -> if acc then acc else f curr ) False

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

Genera una lista alternada de elementos del 1 al 100 de forma [1, -2,..]

>>> listaNegPos
[1,-2,3,-4,5,-6,7,-8,9,-10,11,-12,13,-14,15,-16,17,-18,19,-20,21,-22,23,-24,25,-26,27,-28,29,-30,31,-32,33,-34,35,-36,37,-38,39,-40,41,-42,43,-44,45,-46,47,-48,49,-50,51,-52,53,-54,55,-56,57,-58,59,-60,61,-62,63,-64,65,-66,67,-68,69,-70,71,-72,73,-74,75,-76,77,-78,79,-80,81,-82,83,-84,85,-86,87,-88,89,-90,91,-92,93,-94,95,-96,97,-98,99,-100]

-}
listaNegPos = foldl (\xs x  -> xs ++ [x, -(x + 1)]) [] [1, 3 .. 99]

-- =====================================================
-- * EJERCICIO 3
-- =====================================================
listaRepetidos ::  [[Int]]
{-|
= Descripción

Genera una lista de listas cuyo contenido sera el numero x repetido x veces

>>> take 5 (listaRepetidos)
[[],[1],[2,2],[3,3,3],[4,4,4,4]]

-}
listaRepetidos = map (\x -> take x (repeat x) ) [0..]

-- =====================================================
-- * EJERCICIO 4
-- =====================================================
listaParejas :: Integral a => [(a, a)]
{-|
= Descripción

Genera parejas ordenadas que cumplen x + y = z, en orden de z

>>> take 10 (listaParejas)
[(0,0),(0,1),(1,0),(0,2),(1,1),(2,0),(0,3),(1,2),(2,1),(3,0)]
-}
listaParejas = [(x, y) | sumaTotal <- [0 ..], x <- [0 .. sumaTotal], y <- [0 .. sumaTotal], x + y == sumaTotal]

-- =====================================================
-- * EJERCICIO 5
-- =====================================================
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


-- =====================================================
-- * EJERCICIO 6
-- =====================================================
cuestaPos :: (Ord a) => [a] -> [(a, Int)]
{-|
= Descripción

Devuelve una lista con los elementos considerados cuestas y su posicion

== Ejemplo

>>> cuestaPos [-3,-2,1,0,-1,1]
[(-2,1),(1,2),(1,5)]
-}
cuestaPos (x:xs) = tail ys
  where
     ys = foldl (\res curr -> if esCuesta (fst(head res)) curr then meterCuesta res curr else actualizarCabecera res curr) [(x, 0)] xs


actualizarCabecera :: [(a, Int)] -> a -> [(a,Int)]
{-|

= Descripción

Devuelve una lista con la cabecera actualizada

== Ejemplo

>>> actualizarCabecera [(3,0)] 2
[(2,1)]
-}
actualizarCabecera res curr = (curr, snd(head res) + 1) : tail res

meterCuesta :: [(a, Int)] -> a -> [(a,Int)]
{-|

= Descripción

Añade una cuesta al listado y actualiza la cabecera

== Ejemplo

>>> meterCuesta [(3,0)] 2
[(2,1),(2,1)]
-}
meterCuesta res curr = actualizarCabecera res curr ++ [(curr, snd(head res) + 1)]

esCuesta :: (Ord a) => a -> a -> Bool
{-|

= Descripción

comprueba si un elemento es cuesta

== Ejemplo

>>> esCuesta 1 0
False

>>> esCuesta 2 5
True
-}
esCuesta res curr =  res < curr
