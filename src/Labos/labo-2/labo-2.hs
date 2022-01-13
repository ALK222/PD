-- ALUMNOS: ALEJANDRO BARRACHINA ARGUDO
--          CARLOS MURCIA MORILLA

module Labo2 where
  -- Ejercicio 1
  -- A

  cuadrados :: (Num b, Enum b) => b -> [b]
  cuadrados n = map (\x -> x * x) [0 .. n]

  -- B
  cuadradoInverso :: (Num a, Enum a) => a -> [(a, a)]
  cuadradoInverso n = map (\x -> (x, x * x)) [n, n -1 .. 0]

  -- C
  rsumcos :: (Floating a, Enum a) => a -> a
  rsumcos n = sum (map (\x -> x * abs (cos x)) [1 .. n])

  -- D
  sumMenores :: Integral a => a -> a
  sumMenores n = sum (filter (\x -> (mod x 3 == 0 || mod x 5 == 0)) [1 .. n])

  -- E

  siguientePrimo :: Integral a => a -> a
  siguientePrimo n = head (filter (\x -> isPrime x) [n + 1 ..])

  factores :: Integral a => a -> [a]
  factores n = [x | x <- [1 .. n], mod n x == 0]

  isPrime :: Integral a => a -> Bool
  isPrime n = factores n == [1, n]

  -- Ejercicio 2
  -- A
  iguales :: Eq b => (a -> b) -> (a -> b) -> a -> a -> Bool
  iguales f g n m = y
    where
      xs = map f [n, m]
      ys = map g [n, m]
      y = all (== True) (zipWith (==) xs ys)

  -- B
  menor :: (Enum a, Num a) => a -> (a -> Bool) -> a
  menor n p = y
    where
      y = head (filter p [n, n + 1 ..])

  -- C
  mayorA :: Enum a => a -> a -> (a -> Bool) -> a
  mayorA n m p = y
    where
      y = head (filter p [n .. m])

  -- D
  ex :: Enum a => a -> a -> (a -> Bool) -> Bool
  ex n m p = y
    where
      y = length (filter p [n .. m]) > 0

  -- Ejercicio 3
  -- A
  filter2 :: [a] -> (a -> Bool) -> (a -> Bool) -> ([a], [a])
  filter2 xs p q = (us, vs)
    where
      us = filter p xs
      vs = filter q xs

  -- B
  filters :: [a] -> [a -> Bool] -> [[a]]
  filters xs ys = map (\f -> filter f xs) ys

  -- C
  mapx :: t -> [t -> b] -> [b]
  mapx x fs = map (\f -> f x) fs
