{-|
Module      : Labo5
Description : Laboratorio 5 PD FDI UCM 22-23
License     : GPL-3.0
Maintainer  : Alejandro Barrachina Argudo
Stability   : experimental
Portability : unknown
-}
module Labo5 where

-- =====================================================
-- * EJERCICIO 1
-- =====================================================

getInt :: IO Int
{-|
= Descripción

Coge un número desde la entrada.
-}
getInt = do
  line <- getLine
  return (read line :: Int)

adivina :: Int -> IO ()
{-|
= Descripión

Pide un número por teclado a adivinar. Esto es bastante estúpido de por si solo porque tienes que introducir el número como argumento
-}
adivina x = do
  putStrLn "Intenta adivinar el número"
  numberTried <- getInt
  if numberTried == x
    then putStrLn "Adivinaste el número"
    else
      if numberTried < x
        then do
          putStrLn "El número a adivinar es mayor"
          adivina x
        else do
          putStrLn "El número a adivinar es menor"
          adivina x

-- =====================================================
-- * EJERCICIO 2
-- =====================================================

-- | Representación de una matriz como lista de listas
type Matriz = [[Float]]

getFloat :: IO Float
{-|
= Descripción

Coge un número desde la entrada.
-}
getFloat = do
  line <- getLine
  return (read line :: Float)

-- ** A)
getMatriz :: IO Matriz
{-|
= Descripción

Coge una matriz desde la entrada.

Ver 'getDatosMatriz' para ver el funcionamiento total
-}
getMatriz = do
  putStrLn "Número de filas:"
  filas <- getInt
  putStrLn "Número de columnas:"
  columnas <- getInt
  getDatosMatriz filas columnas

getFilas :: Int -> IO [Float]
{-|
= Descripción

Coge una fila de una matriz según el número de columnas dadas.
-}
getFilas numColumnas =
  if numColumnas == 1
    then do
      num <- getFloat
      return [num]
    else do
      num <- getFloat
      resto <- getFilas (numColumnas - 1)
      return (num : resto)

getDatosMatriz :: Int -> Int -> IO Matriz
{-|
= Descripción

Coge todas las filas de una matriz según el número de filas y columnas dadas.

Ver 'getFilas' para el funcionamiento completo
-}
getDatosMatriz filas columnas =
  if filas == 1
    then do
      fila <- getFilas columnas
      return [fila]
    else do
      fila <- getFilas columnas
      restoFilas <- getDatosMatriz (filas -1) columnas
      return (fila : restoFilas)

-- ** B)

dibujaMatriz :: Matriz -> IO ()
{-|
= Descripción

Dibuja una matriz dada como argumento.
-}
dibujaMatriz [] = putStr ""
dibujaMatriz m =
  if null (head m)
    then do
      putStrLn "\n"
      dibujaMatriz (tail m)
    else do
      putStr (show (head (head m)) ++ " ")
      dibujaMatriz (tail (head m) : tail m)

mostrarMatrizNueva :: IO ()
{-|
= Descripción

Pide una matriz por teclado y la muestra una vez construida.

Ver 'getMatriz' y 'dibujaMatriz'.
-}
mostrarMatrizNueva = do
  m <- getMatriz
  dibujaMatriz m

-- =====================================================
-- * EJERCICIO 3
-- =====================================================

formatea :: String -> String -> Int -> IO ()
{-|
= Descripción

Formatea a n columnas cada linea de un fichero dado en otro.

Ver 'formatear' para el funcionamiento.
-}
formatea fileIn fileOut n = do
  contenido <- readFile fileIn
  writeFile fileOut (formatear n contenido)

-- Funcion que formatea el contenido del fichero
formatear :: Int -> String -> String
{-|
= Descripción

Formatea cada linea a un total de n caracteres por fila.

Ver 'justify' para el funcionamiento completo.
-}
formatear n strIn = unlines (map (justify n) (lines strIn))

-- Funcion que formatea una linea
justify :: Int -> String -> String
{-|
= Descripción

Añade espacios a una línea si es necesario.

Ver 'addSpaces' para el funcionamiento completo.
-}
justify n xs
  | length (words xs) == 1 = xs
  | length xs >= n = xs
  | otherwise = concatMap (addSpaces m) (words xs)
  where
    m = div (n - sum (map length (words xs))) (length (words xs) - 1)

addSpaces :: Int -> String -> String
{-|
= Descripción

Añade n espacios a una lina dada.
-}
addSpaces n p = p ++ [' ' | n <- [1 .. n]]
