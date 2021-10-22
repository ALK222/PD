{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- ALUMNOS: ALEJANDRO BARRACHINA ARGUDO
--          CARLOS MURCIA MORILLA

-- EJERCICIO 1
-- A
last' :: [a] -> a
last' (x : xs) = foldl (\_ curr -> curr) x xs

-- B
foldReverse :: [a] -> [a]
foldReverse = foldl (\curr next -> next : curr) []

-- C
all' :: (a -> Bool) -> [a] -> Bool
all' f = foldl (\acc curr -> f curr && acc) True

-- D
min' :: Ord a => [a] -> a
min' (x : xs) = foldl (\minimun curr -> if curr < minimun then curr else minimun) x xs

-- E
map' :: (a -> b) -> [a] -> [b]
map' filtro = foldl (\x y -> x ++ [filtro y]) []

-- F
filter' :: (a -> Bool) -> [a] -> [a]
filter' filtro = foldl (\x y -> if filtro y then x ++ [y] else x) []

-- G
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' filtro = foldr (\x y -> if filtro x then x : y else []) []

-- EJERCICIO 2
listaNegPos :: [Integer]
listaNegPos = [m | n <- [1 .. 100], m <- [n, negate n]]

-- EJERICICO 3
-- listaTuplas = [m | x <- [0 ..], y <- [0 ..], curr <- [0 ..], m <- if x + y == curr then (x, y) else [()]]

-- EJERICIO 4
-- A
sufijos :: [a] -> [[a]]
sufijos [] = []
sufijos xs = [m | n <- tail xs, m <- n]
