{-|
Module      : Labo22022
Description : Laboratorio 2 PD FDI UCM 22-23
License     : GPL-3.0
Maintainer  : Alejandro Barrachina Argudo y Carlos Murcia Morilla
Stability   : experimental
Portability : unknown
-}
module Labo22022
where

-- >>> listaPares 9
-- [0,2,4,6,8]
listaPares :: (Integral a) => a -> [a]
listaPares x = map (*2) [0.. (x `div` 2)]

-- >>> listaParesCuadrados 3
-- [(3,9),(2,4),(1,1),(0,0)]
listaParesCuadrados :: (Integral a) => a -> [(a,a)]
listaParesCuadrados x = let
                           ys = map (^2) [x, x-1 .. 0]
                           xs = [x, x-1 .. 0]
                        in zip xs ys

-- >>> listaPotencias 3
-- [1,3,9]
listaPotencias :: (Integral a) =>Int -> [a]
listaPotencias x = take x (iterate (*3) 1)

-- >>> sumaMenores 8
-- 14
sumaMenores :: Int -> Int
sumaMenores x = let xs = filter (\x -> mod x 3 == 0 || mod x 5 == 0) [0..x]
                in sum xs

-- >>> primerPrimoMayor 38
-- 41
primerPrimoMayor :: (Integral a) => a -> a
primerPrimoMayor x = head $ filter isPrime [x, x+1 ..]

-- >>> isPrime 5
-- True
isPrime :: (Integral a) => a -> Bool
isPrime x
  | x < 2 = False
  | otherwise = x == head (filter (\a -> x `mod` a == 0 ) [2 .. x])

-- >>> iguales primerPrimoMayor sumaMenores  0 10
-- False

-- >>> iguales sumaMenores sumaMenores  0 10
-- True
iguales :: Eq b => (a -> b) -> (a -> b) -> a -> a -> Bool
iguales f g n m = y
  where
    xs = map f [n, m]
    ys = map g [n, m]
    y = all (== True) (zipWith (==) xs ys)

-- >>> menorA 0 100 isPrime
-- 2

-- >>> menorA 20 22 isPrime
-- Prelude.head: empty list
menorA :: Integral a => a -> a -> (a -> Bool) -> a
menorA n m p = y
  where
    y = head (filter p [n .. m])

-- >>> menor 2077 isPrime
-- 2081

-- >>> menor 2077 (<2)
-- Bucle infinito
menor :: (Num p, Enum p) => p -> (p -> Bool) -> p
menor n p = y
  where
    y = head (filter p [n ..])

-- >>> mayorA 0 100 isPrime
-- 97

-- >>> mayorA 20 22 isPrime
-- Prelude.last: empty list
mayorA :: Enum p => p -> p -> (p -> Bool) -> p
mayorA n m p = y
  where
    y = last (filter p [n .. m])

-- >>> pt 2 3 isPrime
-- True

-- >>> pt 2 5 isPrime
-- False
pt :: Enum p => p -> p -> (p -> Bool) -> Bool
pt n m p = y
  where
    y = length (filter p [n .. m]) == length [n .. m]

-- >>> filter2 [1..100] (+1) (+2)
-- [[2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101],[3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102]]

filter2 :: [a] -> (a -> b) -> (a -> b) -> [[b]]
filter2 xs f g = [us, vs]
  where
    us = map f xs
    vs = map g xs


-- >>> partition isPrime [1..10]
-- ([2,3,5,7],[1,4,6,8,9,10])
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = (us, vs)
  where
    us = filter p xs
    vs = filter (not . p) xs

mapx :: t -> [t -> b] -> [b]
mapx x = map (\f -> f x)

-- >>> filter1 [[1,2], [3,4], [5,6]] isPrime
-- [[2],[3],[5]]
filter1 :: [[a]] -> (a -> Bool) -> [[a]]
filter1 xss p = map (filter p) xss

-- >>> filters [1..5] [isPrime, (<3)]
-- [[2,3,5],[1,2]]
filters :: [a] -> [(a -> Bool)] -> [[a]]
filters xs ps = map (\p -> filter p xs) ps
