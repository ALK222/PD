{-|
Module      : Traductor
Description : Sistema de Traducción simple
License     : GPL-3.0
Maintainer  : Alejandro Barrachina Argudo
Stability   : experimental
Portability : unknown

Práctica final de Programación Declarativa (PD) de Ingeniería Infromática de la UCM
-}
module Traductor (
  -- * Types
  NodeElement,
  Traduction,
  Bucket,
  Hash,
  -- * Functions
  -- ** Generators
  leaf,
  genEmptyTree,

  -- ** Tree and List manipulators
  insert,
  insertList,
  inorder,
  busqueda,
  busquedaLista,
  evens,
  odds,

  -- ** Utilities
  hash,
  calcMeanWordCount,
  calcMeanWordCountExample,
  rellenadoPorFichero,
  rellenadoPorFicheroFijo,

 -- ** IO
  lecturaPalabras,
  mostrarTraducciones,
  demoMode
)
where

-- =====================================================
-- PARTE I: Definicion de la estructura del diccionario
-- =====================================================

-- | Nodo de la tabla de traduccion usando un hash con colisiones con valores [0 .. 9] para guardar valores
data  NodeElement = NE
  Int    -- ^ Índice del elemento
  Bucket -- ^ Contenido del Nodo

-- | Alias para las tuplas usadas para representar traducciones
type Traduction = (String, String)

-- | Elementos de la tabla de traducción asignados a un valor concreto, se guardarán varias traducciones distintas
type Bucket = [Traduction]

-- | Árbol para almacenar todos los nodos, puede ser vacío o un nodo con ramas a ambos lados. Estas ramas también pueden ser vacias
data Hash a
  -- | Nodo nulo del arbol, no alberga información
  = Nil
  -- | Nodo con una raiz y un arbol a cada lado.
  | Node (Hash a) a (Hash a)


leaf :: a -> Hash a
{-|
__Description__


Genera un árbol que solo tiene raiz, una hoja

=== __Examples__

>>> leaf (NE 1 [])
[1] -> []
-}
leaf a = Node Nil a Nil


-- =====================================================
-- PARTE II: Instancia de Show
-- =====================================================

instance Show NodeElement where show (NE i b) = "[" ++ show i ++ "] -> " ++ show b ++ "\n"
instance Show a => Show (Hash a) where
  show Nil = ""
  show t   = inorder t


inorder ::(Show a) => Hash a -> String
{- |
__Description__


Muestra el inorden del árbol dado
-}
inorder Nil            = []
inorder (Node t1 v t2) = inorder t1 ++ show v ++ inorder t2

-- =====================================================
-- PARTE III: Hash
-- =====================================================

hash :: String -> Int
{-|
__Description__


Hace un hash de una palabra y se queda con el último digito para usarlo de índice

=== __Examples__

>>> hash "Hola"
8

>>> hash "Adios"
0
-}
hash s =  foldl (\h c -> 33*h + fromEnum c) 2424 s `mod` 10



-- =====================================================
-- PARTE IV: Generación y población del árbol
-- =====================================================

genEmptyTree :: [Int] -> Hash NodeElement
{-|
__Description__


Genera un árbol binario de n elementos en una lista para meter las palabras dadas

-}
genEmptyTree [] = Nil
genEmptyTree nodes = Node (genEmptyTree $ take half nodes)
                     (NE (nodes !! half) [])
                     (genEmptyTree $ drop (half + 1) nodes)
                     where
                       half = length nodes `div` 2

    -- Node (Node (Node (leaf (NE 0 [] )) (NE 1 []) (leaf (NE 2 []))) (NE 3 []) (leaf (NE 4[]))) -- Rama izquierda
    -- (NE 5 []) -- Cima
    -- (Node Nil (NE 6 []) (Node (leaf (NE 7 [])) (NE 8 []) (leaf (NE 9 [])))) -- Rama derecha


insert :: Hash NodeElement -> Int -> Traduction -> Hash NodeElement
{-|
__Description__


Inserta un elemento en el indice dado

=== __Examples__

>>> insert (Node (leaf (NE 0 [])) (NE 1 []) Nil) 1 ("a","b")
[0] -> []
[1] -> [("a","b")]

>>> insert Nil 1 ("a","b")
[1] -> [("a","b")]
-}
insert Nil x t = leaf (NE x [t]) -- Añadimos un Nodo al arbol vacio
insert (Node t1 (NE i1 b1) t2)  x t
  | i1 ==  x = Node t1 (NE i1 (insertList b1 t)) t2 --  Añadimos la traducción al listado
  | i1  <  x = Node t1 (NE i1 b1) (insert t2 x t) -- Buscamos en el lado  derecho
  | i1  > x = Node (insert t1 x t) (NE i1 b1) t2 -- Buscamos en el lado izquierdo


insertList :: [Traduction] -> Traduction -> [Traduction]
{-|
__Description__


Inserción ordenada de traducciones en una lista.
-}
insertList [] t = [t]
insertList (x : xs) t
  | fst t <= fst x = t:x:xs
  | otherwise = x : insertList xs t
-- =====================================================
-- PARTE V: Lectura de palabras por teclado
-- =====================================================

lecturaPalabras :: String -> IO String
{-|
__Description__


Lee palabras por el teclado y devuelve un String con todas las palabras introducidas
-}
lecturaPalabras message = do
  putStrLn message
  getLine

rellenadoPorFichero :: IO (Hash NodeElement)
{-|
__Description__


Pide un fichero para generar el árbol de traducciones
-}
rellenadoPorFichero = do
  let tree = genEmptyTree [0..9] -- generación del arbol vacío
  path <- lecturaPalabras "Introduzca la ruta del archivo de traducciones: "
  palabras <- readFile path
  let traducciones = odds $ zip (words palabras) (tail (words palabras))-- Generación de un listado de traducciones
  let fullTree = foldl (\tr t -> insert tr (hash (fst t)) t) tree traducciones -- Población del árbol
  return fullTree


rellenadoPorFicheroFijo :: IO (Hash NodeElement)
{-|
__Description__


Lectura del fichero "datos.txt" para cargar un arbol de traducciones
-}
rellenadoPorFicheroFijo = do
  let tree = genEmptyTree [0..9] -- generación del arbol vacío
  palabras <- readFile "datos.txt"
  let traducciones = odds $ zip (words palabras) (tail (words palabras))-- Generación de un listado de traducciones
  let fullTree = foldl (\tr t -> insert tr (hash (fst t)) t) tree traducciones -- Población del árbol
  return fullTree

-- =====================================================
-- PARTE VI: Cálculo de media de letras por palabras
-- =====================================================

calcMeanWordCount ::  String ->  Float
{-|
__Description__


Caclula la media de letras por palabra en una frase.

Usa foldl para recorrer cada una de las palabras y añadir sus letras al numero total

Mas tarde se divide entre el numero de palabras para sacar la media de letras sin contar los espacios en blanco

=== __Examples__

>>> calcMeanWordCount "a b c d"
1

>>> calcMeanWordCount "ABCD"
4
-}
calcMeanWordCount s = fromIntegral (foldl (\total palabra -> total + length palabra) 0 (words s)) / fromIntegral (length (words s))


calcMeanWordCountExample :: String -> IO ()
{- |
__Description__


Coje el texto del fichero dado a la función y calcula la media de letras por palabra.

=== __Examples__

>>> calcMeanWordCountExample "./pruebas/quijote.txt"
4.5021253

>>> calcMeanWordCountExample "./pruebas/call_of_cthulhu.txt"
4.8737144
-}
calcMeanWordCountExample file = do
    text <- readFile file
    print (calcMeanWordCount text)


-- =====================================================
-- PARTE VII: Busqueda de palabras en el arbol
-- =====================================================
busqueda :: Hash NodeElement -> Int -> String -> String
{-|
__Description__


Busca una palabra en el listado dentro del árbol dado.

Usa el índice para encontrar el nodo del listado

=== __Examples__

>>> busqueda (insert genEmptyTree (hash "hola") ("hola", "hello")) (hash "hola") "hola"
"hello"

>>> busqueda (genEmptyTree) (hash "hola") "hola"
"*** Exception: Palabra no encontrada
CallStack (from HasCallStack):
  error, called at hash.hs:190:28 in main:Traductor
-}
busqueda Nil i s = "Palabra no encontrada"
busqueda (Node iz (NE index v) der) i s
  | index == i = busquedaLista v s
  | index < i = busqueda der i s
  | otherwise = busqueda iz i s

busquedaLista :: [Traduction] -> String -> String
{-|
__Description__


Busqueda de una palabra en un listado de traducciones para conseguir la traducción.

=== __Examples__

>>> busquedaLista [] "hola"
"Palabra no encontrada"

>>> busquedaLista [("Hola", "Hello"), ("A", "B")] "Hola"
"Hello"
-}
busquedaLista [] _       = "Palabra no encontrada"
busquedaLista (x : xs) s = if fst x == s then snd x else busquedaLista xs s

-- =====================================================
-- PARTE VIII: Mostrar las traducciones por pantalla
-- =====================================================

evens :: [a] -> [a]
{-|
__Description__

Función auxiliar de 'odds' para eliminar los indices pares de una lista
-}
evens []     = []
evens (_:xs) = odds xs

odds :: [a] -> [a]
{-|
__Description__

Elimina los indices pares de una lista
-}
odds []     = []
odds (x:xs) = x : evens xs

mostrarTraducciones :: IO ()
{-|
__Description__


Traduce las palabras dadas por el usuario por teclado.

Genera un árbol vacío, lo llena con un archivo de traducciones y lo usa para traducir una serie de palabras dadas
-}
mostrarTraducciones = do

  fullTree <- rellenadoPorFicheroFijo
  solicitud <- lecturaPalabras "Introduzca las palabras a traducir (separadas por espacios y sin \")"
  let solicitudes = words solicitud
  let resT = [busqueda fullTree (hash t) t | t <- solicitudes] -- Listado de traducciones encontradas (o no encontradas)

  let resList = zip solicitudes resT -- Listado (solicitud, traducción)
  putStrLn "\nRESULTADO:\n"
  putStrLn (foldl (\s t -> s ++ fst t ++ " -> " ++ snd t ++ "\n") [] resList)

-- =====================================================
-- PARTE IX: Modo demo
-- =====================================================

demoMode :: IO ()
{-|
__Description__

Modo para probar todas las funciones implementadas en el módulo

-}
demoMode = do
  putStrLn "[MODO DE PRUEBA DE TODAS LAS FUNCIONES]"

  fullTree <- rellenadoPorFichero
  nuevasPalabras <- lecturaPalabras "Introduzca Traducciones para meter en el diccionario (separadas por espacios, de orden palabra traducción y sin \")"
  let nuevasTraducciones = odds $ zip (words nuevasPalabras) (tail (words nuevasPalabras))
  let fullTree1 = foldl (\tr t -> insert tr (hash $ fst t) t) fullTree nuevasTraducciones
  solicitud <- lecturaPalabras "Introduzca las palabras a traducir (separadas por espacios)"

  let solicitudes = words solicitud
  let resT = [busqueda fullTree1 (hash t) t | t <- solicitudes] -- Listado de traducciones encontradas (o no encontradas)

  let resList = zip solicitudes resT -- Listado (solicitud, traducción)
  putStrLn "\nRESULTADO:\n"
  putStrLn "\n[TRADUCCIONES]\n"
  putStrLn (foldl (\s t -> s ++ fst t ++ " -> " ++ snd t ++ "\n") [] resList)
  putStrLn "\n[MEDIA DE LETRAS POR PALABRA]\n"
  print (calcMeanWordCount solicitud)
  putStrLn "\n[DICCIONARIO TRAS INSERCIONES]\n"
  print fullTree1
