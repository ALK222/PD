-- ALUMNOS: ALEJANDRO BARRACHINA ARGUDO
--          CARLOS MURCIA MORILLA

-- EJERCICIO 1

getInt :: IO Int
getInt = do
  line <- getLine
  return (read line :: Int)

adivina :: Int -> IO ()
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

-- EJERCICIO 2
type Matriz = [[Float]]

getFloat :: IO Float
getFloat = do
  line <- getLine
  return (read line :: Float)

-- A
getMatriz :: IO Matriz
getMatriz = do
  putStrLn "Número de filas:"
  filas <- getInt
  putStrLn "Número de columnas:"
  columnas <- getInt
  getDatosMatriz filas columnas

getFilas :: Int -> IO [Float]
getFilas numColumnas = do
  if (numColumnas == 1)
    then do
      num <- getFloat
      return [num]
    else do
      num <- getFloat
      resto <- getFilas (numColumnas - 1)
      return (num : resto)

getDatosMatriz :: Int -> Int -> IO Matriz
getDatosMatriz filas columnas = do
  if (filas == 1)
    then do
      fila <- getFilas columnas
      return [fila]
    else do
      fila <- getFilas columnas
      restoFilas <- getDatosMatriz (filas -1) columnas
      return (fila : restoFilas)

-- B

dibujaMatriz :: Matriz -> IO ()
dibujaMatriz [] = putStr ""
dibujaMatriz m = do
  if head m == []
    then do
      putStrLn "\n"
      dibujaMatriz (tail m)
    else do
      putStr (show (head (head m)) ++ " ")
      dibujaMatriz (tail (head m) : tail m)

mostrarMatrizNueva :: IO ()
mostrarMatrizNueva = do
  m <- getMatriz
  dibujaMatriz m
