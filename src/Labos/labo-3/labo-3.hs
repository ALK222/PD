{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- ALUMNOS: ALEJANDRO BARRACHINA ARGUDO
--          CARLOS MURCIA MORILLA

-- EJERCICIO 1
-- A
last' :: [a] -> a
last' (x : xs) = foldl (\_ curr -> curr) x xs

-- B
reverse' :: [a] -> [a]
reverse' = foldl (\curr next -> next : curr) []

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
listaParejas' :: (Num b, Enum b, Eq b) => b -> [(b, b)]
listaParejas' n = [(x, y) | sumaTotal <- [0 .. n], x <- [0 .. sumaTotal], y <- [0 .. sumaTotal], x + y == sumaTotal]

listaParejas :: Integral a => [(a, a)]
listaParejas = [(x, y) | sumaTotal <- [0 ..], x <- [0 .. sumaTotal], y <- [0 .. sumaTotal], x + y == sumaTotal]

-- EJERICIO 4
-- A
sufijos :: [a] -> [[a]]
sufijos xs = reverse' [drop (length xs - n) xs | n <- [0 .. length xs]]

-- B
sublistas :: [a] -> [[a]]
sublistas [] = []
sublistas xs = [take n (drop i xs) | n <- [1 .. (length xs)], i <- [0 .. (length xs) - 1]]
