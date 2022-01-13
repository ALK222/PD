-- ALUMNOS: ALEJANDRO BARRACHINA ARGUDO
--          CARLOS MURCIA MORILLA
module Labo3 where
  -- EJERCICIO 1
  -- A
  last' :: [a] -> a
  last' []       = error "Lista vacia"
  last' (x : xs) = foldl (\_ curr -> curr) x xs

  -- B
  reverse' :: [a] -> [a]
  reverse' = foldl (\curr next -> next : curr) []

  -- C
  all' :: (a -> Bool) -> [a] -> Bool
  all' f = foldl (\acc curr -> f curr && acc) True

  -- D
  min' :: Ord a => [a] -> a
  min' []       = error "Lista vacia"
  min' (x : xs) = foldl (\minimun curr -> min minimun curr) x xs

  -- E
  map' :: (a -> b) -> [a] -> [b]
  map' filtro = foldr (\x xs -> filtro x : xs) []

  -- F
  filter' :: (a -> Bool) -> [a] -> [a]
  filter' filtro = foldr (\x xs -> if filtro x then x : xs else xs) []

  -- G
  takeWhile' :: (a -> Bool) -> [a] -> [a]
  takeWhile' filtro = foldr (\x y -> if filtro x then x : y else []) []

  -- EJERCICIO 2
  -- listaNegPos :: [Integer]
  -- listaNegPos = [m | n <- [1 .. 100], m <- [n, negate n]]
  listaNegPos :: [Integer]
  listaNegPos = foldl (\xs x -> xs ++ [x, - x]) [] [1 .. 100]

  -- EJERICICO 3
  listaParejas' :: (Num b, Enum b, Eq b) => b -> [(b, b)]
  listaParejas' n = [(x, y) | sumaTotal <- [0 .. n], x <- [0 .. sumaTotal], y <- [0 .. sumaTotal], x + y == sumaTotal]

  listaParejas :: Integral a => [(a, a)]
  listaParejas = [(x, y) | sumaTotal <- [0 ..], x <- [0 .. sumaTotal], y <- [0 .. sumaTotal], x + y == sumaTotal]

  -- EJERICIO 4
  -- A
  sufijos :: [a] -> [[a]]
  sufijos xs = [drop x xs | x <- [0 .. length xs]]

  -- B
  sublistas :: [a] -> [[a]]
  sublistas [] = []
  sublistas xs = [take n (drop i xs) | n <- [1 .. (length xs)], i <- [0 .. length xs - 1], length (drop i xs) >= n]

  -- C
  permuta :: [a] -> [[a]]
  permuta []       = [[]]
  permuta (x : xs) = concat [intercalado x ys | ys <- permuta xs]

  intercalado :: a -> [a] -> [[a]]
  intercalado x []       = [[x]]
  intercalado x (y : ys) = (x : y : ys) : [y : zs | zs <- intercalado x ys]

  -- D
  sumandos :: (Integral a) => a -> [[a]]
  sumandos n = sumandos' n 0 1

  sumandos' :: (Integral a) => a -> a -> a -> [[a]]
  sumandos' n x i
    | x == n = [[]]
    | otherwise = [z : zs | z <- [y | y <- [1 .. n], y + x <= n, y >= i], zs <- sumandos' n (z + x) z]
