{-# LANGUAGE InstanceSigs #-}
{-|
Module      : Lote3
Description : Lote de ejercicios 2 PD FDI UCM 23-24
License     : GPL-3.0
Maintainer  : Alejandro Barrachina Argudo
Stability   : experimental
Portability : unknown
-}
module Lote3
where

-- =====================================================
-- * EJERCICIO 1
-- =====================================================

-- | Representación de un numero complejo
data Complejo = C Float Float deriving (Eq -- ^ Derivación estandar de Eq
                                        )

-- | Representa el número como "a (+-) bi"
instance Show Complejo where show :: Complejo -> String
                             show (C a b) = if b > 0 then show a ++ "+" ++ show b ++ "i" else show a ++ show b ++ "i"

-- | Implementación especial para '(+)' '(-)' y '(*)'
instance Num Complejo where
  (+) :: Complejo -> Complejo -> Complejo
  a + b = case (a, b) of (C x y, C x1 y1) -> C (x + x1) (y + y1)
  (-) :: Complejo -> Complejo -> Complejo
  a - b = case (a, b) of (C x y, C x1 y1) -> C (x - x1) (y - y1)
  (*) :: Complejo -> Complejo -> Complejo
  a * b = case (a, b) of (C x y, C x1 y1) -> C ((x * x1) - (y * y1)) ((x * y1) + (y * x1))

-- =====================================================
-- * EJERCICIO 2
-- =====================================================

data Arbol a = N a [Arbol a] deriving (Eq)

instance (Ord a) => Ord (Arbol a) where
    compare (N a1 _) (N a2 _) = compare a1 a2
instance (Show a) => Show (Arbol a) where
    show (N x ts) = showTree 0 3 (N x ts) where

showTree k j (N x ts) = take k (repeat ' ') -- k espacios  
                ++ "N " ++ show x  ++ "\n"
                ++ concat (map (showTree (k+length ("N " ++ show x) + j) j) ts)

listaHojas :: Arbol a -> [a]
listaHojas (N x [])    = [x]
listaHojas (N _ hijos) = concat $ map listaHojas hijos

listaNodos :: Arbol a -> [a]
listaNodos (N x []) = [x]
listaNodos (N x hijos) =  x : concat (map listaNodos hijos)

repMax:: Ord a => Arbol a -> Arbol a
repMax t = rep m t where
  m = maximum (listaNodos t)
  rep:: a -> Arbol a -> Arbol a
  rep x (N _ ts) = N x (map (rep x) ts)

-- =====================================================
-- * EJERCICIO 3
-- =====================================================


-- | Clase para medir otros tipos de datos
class Medible a where

  medida :: a -> Int -- ^ Coge el dato dado y saca una medida de él

-- | Mediremos True como 0  y false como 1
instance Medible Bool where
  medida True  = 0
  medida False = 1

-- | Suma todas las medidas de la lista
instance (Medible a) => Medible [a] where
  medida []       = 0
  medida (x : xs) = medida x + medida xs

-- | Suma la medida de los dos elementos dados
instance (Medible a, Medible b) => Medible (a, b) where
  medida (x, y) = medida x + medida y

-- =====================================================
-- * EJERCICIO 4
-- =====================================================

data Exp = Num Int | Mas Exp Exp | Menos Exp Exp | Por Exp Exp | Div Exp Exp deriving Show

-- eval e = resultado de evaluar e
eval:: Exp -> Int
eval (Num n) = n
eval (Mas e1 e2)   = eval e1 + eval e2
eval (Menos e1 e2) = eval e1 - eval e2
eval (Por e1 e2)   = eval e1 * eval e2
eval (Div e1 e2)   = eval e1 `div` eval e2  -- división entera

-- cuentaOps e = número de operaciones aritméticas en e
cuentaOps:: Exp -> Int
cuentaOps (Num n) = 0
cuentaOps (Mas e1 e2)   = 1 + cuentaOps e1 + cuentaOps e2
cuentaOps (Menos e1 e2) = 1 + cuentaOps e1 - cuentaOps e2
cuentaOps (Por e1 e2)   = 1 + cuentaOps e1 * cuentaOps e2
cuentaOps (Div e1 e2)   = 1 + cuentaOps e1 `div` cuentaOps e2

-- cambiaOps e = resultado de cambiar en e ‘más´ por ‘menos’ y ‘por’ por ‘entre’
cambiaOps:: Exp -> Exp
cambiaOps (Num n) = (Num n)
cambiaOps (Mas e1 e2)   = Menos (cambiaOps e1) (cambiaOps e2)
cambiaOps (Menos e1 e2) = Menos (cambiaOps e1) (cambiaOps e2)
cambiaOps (Por e1 e2)   = Div (cambiaOps e1) (cambiaOps e2)
cambiaOps (Div e1 e2)   = Menos (cambiaOps e1) (cambiaOps e2)

-- operandos e = lista de los enteros que aparecen en e
operandos:: Exp -> [Int]
operandos (Num n) = [n]
operandos (Mas e1 e2)   = operandos e1 ++ operandos e2
operandos (Menos e1 e2) = operandos e1 ++ operandos e2
operandos (Por e1 e2)   = operandos e1 ++ operandos e2
operandos (Div e1 e2)   = operandos e1 ++ operandos e2

-- =====================================================
-- * EJERCICIO 5
-- =====================================================

{-|
Representación de un punto de forma (x,y)

-}
data Punto
  = P -- ^ Constructor del punto
    Int -- ^ Coordenada X
    Int -- ^ Coordenada Y

pointSum :: (Ord a, Num a) => a -> a -> a
{-|
= Descripción

Suma un numero a una parte de una coordenada dada. Tiene como rango [0 .. 100]

== Ejemplos

>>> pointSum 100 10
9

>>> pointSum 0 (-1)
100

>>>pointSum 100 1
0
-}
pointSum a b
  | a + b > 100 = (a + b - 1) - 100
  | a + b < 0 = 100 - ( a + b + 1)
  | otherwise = a + b

-- | Representación de una dirección
data Direccion = ARRIBA | ABAJO | IZQUIERDA | DERECHA deriving (
  Eq      -- ^ Derivación estandar de Eq
  , Ord   -- ^Derivación estandar de Ord
  , Show  -- ^ Derivación estandar de Show
  )

-- | Muestra el punto como "(a,b)""
instance Show Punto where show (P a b) = "(" ++ show a ++ "," ++ show b ++ ")"

-- ** A)
mueve :: Punto -> Direccion -> Punto
{-|
= Descripción

Mueve un punto en la dirección dada

== Ejemplos

>>> mueve (P 0 1) IZQUIERDA
(100,1)

>>> mueve (P 0 1) DERECHA
(1,1)

>>> mueve (P 0 1) ARRIBA
(0,2)

>>> mueve (P 0 1) ABAJO
(0,0)
-}
mueve p ARRIBA    = case p of (P a b) -> P a (pointSum b 1)
mueve p ABAJO     = case p of (P a b) -> P a (pointSum b (negate 1))
mueve p DERECHA   = case p of (P a b) -> P (pointSum a 1) b
mueve p IZQUIERDA = case p of (P a b) -> P (pointSum a (negate 1)) b

-- ** B)

destino :: Punto -> [Direccion] -> Punto
{-|
= Descripción

Mueve un punto en las direcciones dadas

== Ejemplos

>>> destino (P 0 0) [ARRIBA, ARRIBA, ABAJO, ABAJO, IZQUIERDA, DERECHA, IZQUIERDA, DERECHA]
(0,0)

>>> destino (P 0 0) [ARRIBA, ARRIBA, ABAJO, IZQUIERDA, IZQUIERDA, DERECHA]
(100,1)
-}
destino p [] = p
destino p ms = foldl mueve p ms

-- ** C)
trayectoria :: Punto -> [Direccion] -> [Punto]
{-|
= Descripción

Mueve un punto en las direcciones dadas mostrando los puntos por los que se han pasado

== Ejemplos

>>> trayectoria (P 0 0) [ARRIBA, ARRIBA, ABAJO, ABAJO, IZQUIERDA, DERECHA, IZQUIERDA, DERECHA]
[(0,0),(0,1),(0,2),(0,1),(0,0),(100,0),(0,0),(100,0),(0,0)]

>>> trayectoria (P 0 0) [ARRIBA, ARRIBA, ABAJO, IZQUIERDA, IZQUIERDA, DERECHA]
[(0,0),(0,1),(0,2),(0,1),(100,1),(99,1),(100,1)]
-}
trayectoria p = foldl (\dir m -> dir ++ [mueve (last dir) m]) [p]

inferior:: [Direccion] -> [Direccion] -> Bool
inferior dirs1 dirs2 = and $ zipWith debajo (trayectoria (P 0 0) dirs1) (trayectoria ( P 0 0) dirs2)
  where
     debajo:: Punto -> Punto -> Bool
     debajo (P x y) (P x' y') = y <= y'

-- =====================================================
-- * EJERCICIO 6
-- =====================================================

data Conjunto a = Con Int [a] deriving Eq

instance (Ord a) => Ord (Conjunto a) where
  compare (Con card1 _) (Con card2 _) = compare card1 card2


toC :: (Eq a) => [a] -> Conjunto a

toC [] = Con 0 []
toC (x:xs) = newCon where
    Con n l = toC xs
    newCon = if x ` notElem` l then Con (n + 1) (x:l) else Con n l

interseccion :: (Eq a) => Conjunto a -> Conjunto a -> Conjunto a
interseccion (Con n1 l1) (Con n2 l2) = Con card l where
    l = [x | x <- l1, x `elem` l2]
    card = length l

union :: Eq a => Conjunto a -> Conjunto a -> Conjunto a
union (Con _ l1) (Con _ l2) = toC (l1++l2)

mapSet :: (Eq a, Eq b) => (a->b) -> Conjunto a -> Conjunto b
mapSet f (Con n l) = toC (map f l)

-- =====================================================
-- * EJERCICIO 7
-- =====================================================

data Asec a b = Void | Acons a (Asec b a)

long :: Num p => Asec a b -> p
long Void = 0
long (Acons ab abs) = 1 + long abs

nElem :: Int -> Asec a b -> Either a b
nElem 0 (Acons a abs) = Left a
nElem 1 (Acons _ (Acons b abs)) = Right b
nElem n (Acons _ (Acons _ abs)) = nElem (n-2) abs

separa :: Asec a b -> ([a], [b])
separa Void = ([],[])
separa (Acons a Void) = ([a],[])
separa (Acons a (Acons b abs)) = (a:as, b:bs) where (as,bs) = separa abs

instance (Eq a, Eq b) => Eq (Asec a b) where
    (==) :: (Eq a, Eq b) => Asec a b -> Asec a b -> Bool
    sec1 == sec2 = separa sec1 == separa sec2

instance (Ord a, Ord b) => Ord (Asec a b) where
    (<=) :: (Ord a, Ord b) => Asec a b -> Asec a b -> Bool
    sec1 <= sec2 = separa sec2 <= separa sec2

instance (Show a, Show b) => Show (Asec a b) where
    show :: (Show a, Show b) => Asec a b -> String
    show abs = "["++ showSec abs ++ "]"

showSec :: (Show a, Show b) => Asec a b -> String
showSec Void = ""
showSec (Acons a Void) = show a
showSec (Acons a abs) = show a ++ ", " ++ show abs

-- =====================================================
-- * EJERCICIO 8
-- =====================================================

palabras :: String -> IO Int
palabras fileIn = do
    file <- readFile fileIn
    return (length $ words file)

palabras' :: IO ()
palabras' = do
    putStrLn "Introduzca el nombre del fichero"
    putStr "> "
    fileIn <- getLine
    file <- readFile fileIn
    putStrLn ("El fichero " ++ fileIn ++ " tiene " ++ show (length $ words file) ++ " palabras.")

getInt :: IO Int
getInt = do
    i <- getLine
    return (read i)

promedia :: IO ()
promedia = do
    putStrLn "Escribe numeros enteros linea a linea (-1 para terminar)"
    promediaAux []

promediaAux :: [Int] -> IO ()
promediaAux xs = do
    n <- getInt
    if n == (-1)
        then
            return ()
        else
            let ns' = (n:xs)
                suma = sum ns'
                promedio = fromIntegral suma / fromIntegral (length ns')
            in do
                putStrLn ("La suma acumulada es "++ show suma)
                putStrLn ("El promedio hasta ahora es " ++ show promedio)
                promediaAux ns'