{-|
Module      : Lote2
Description : Lote de ejercicios 2 PD FDI UCM 23-24
License     : GPL-3.0
Maintainer  : Alejandro Barrachina Argudo
Stability   : experimental
Portability : unknown
-}
module Lote2
where

-- =====================================================
-- * EJERCICIO 1
-- =====================================================
cuadradosFinitos :: [Integer]
{-|
=Description
Lista de cuadrados del 0 al 50

==Example
>>> cuadradosFinitos
[0,1,4,9,16,25,36,49,64,81,100,121,144,169,196,225,256,289,324,361,400,441,484,529,576,625,676,729,784,841,900,961,1024,1089,1156,1225,1296,1369,1444,1521,1600,1681,1764,1849,1936,2025,2116,2209,2304,2401,2500]
-}
cuadradosFinitos = map (\x -> x*x) [0..50]

cuadradosInfinitos :: [ Integer]
{-|
=Description
Lista de cuadrados del 0 al 50

==Example
>>> cuadradosInfinitos
[0,1,4,9,16,25,36,49,64,81,100,121,144,169,196,225,256,289,324,361,400,441,484,529,576,625,676,729,784,841,900,961,1024,1089,1156,1225,1296,1369,1444,1521,1600,1681,1764,1849,1936,2025,2116,2209,2304,2401,2500]
-}
cuadradosInfinitos = take 51 $ map (\x -> x*x) [0..50]

cuadradosNoCuadrados:: [(Int, Int)]
{-|
=Description
Lista de cuadrados del 0 al 50 con su numero no cuadrado

==Example
>>> cuadradosNoCuadrados
[(50,2500),(49,2401),(48,2304),(47,2209),(46,2116),(45,2025),(44,1936),(43,1849),(42,1764),(41,1681),(40,1600),(39,1521),(38,1444),(37,1369),(36,1296),(35,1225),(34,1156),(33,1089),(32,1024),(31,961),(30,900),(29,841),(28,784),(27,729),(26,676),(25,625),(24,576),(23,529),(22,484),(21,441),(20,400),(19,361),(18,324),(17,289),(16,256),(15,225),(14,196),(13,169),(12,144),(11,121),(10,100),(9,81),(8,64),(7,49),(6,36),(5,25),(4,16),(3,9),(2,4),(1,1),(0,0)]
-}
cuadradosNoCuadrados = [(x, x^2) | x <- [50,49..0]]

sumSen :: Float
{-|
=Description
\sum^{i=100}_{i=1} i * | sen(i)|

==Example
>>> sumSen
3219.2346
-}
sumSen = foldl (\sum x -> sum + (x * abs (sin x))) 0 [1..100]

first50powersof3 :: [Integer]
{-|
=Description

50 powers of 3

==Example
>>>first50powersof3
[1,3,9,27,81,243,729,2187,6561,19683,59049,177147,531441,1594323,4782969,14348907,43046721,129140163,387420489,1162261467,3486784401,10460353203,31381059609,94143178827,282429536481,847288609443,2541865828329,7625597484987,22876792454961,68630377364883,205891132094649,617673396283947,1853020188851841,5559060566555523,16677181699666569,50031545098999707,150094635296999121,450283905890997363,1350851717672992089,4052555153018976267,12157665459056928801,36472996377170786403,109418989131512359209,328256967394537077627,984770902183611232881,2954312706550833698643,8862938119652501095929,26588814358957503287787,79766443076872509863361,239299329230617529590083]
-}
first50powersof3 = take 50 $ iterate (*3) 1

filtered1000powersof3 :: Int
{-|
=Description

Powers of 3 that end in 67

==Example
>>>filtered1000powersof3
10
-}
filtered1000powersof3 = length $ filter (\x -> x `mod` 100 == 67)  $ takeWhile (<(10 ^ 100)) $ iterate (*3) 1

menores1000 :: Integer
{-|
=Description
Sum of numbers bellow 1000 that are multiple of 3 or 5

==Example
>>>menores1000
233168
-}
menores1000 = sum $ filter (\x -> (mod x 3 == 0)||(mod x 5 == 0)) [0..999]

menores1020 :: Integer
{-|
=Description
Numbers bellow 10^20 that are multiple of 3 or 5

==Example
>>>menores1020
2333333333333333333316666666666666666668
-}
menores1020 = let {f n = div (10^20-1) n ; s n = (n + n*f n)*(f n) `div` 2} in s 3 + s 5 - s 15


listaPotencias :: [[Int]]
{-|
=Description

List of powers 1 to 10 of each number 1 to 20

==Example
>>>listaPotencias
[[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],[1,4,9,16,25,36,49,64,81,100,121,144,169,196,225,256,289,324,361,400],[1,8,27,64,125,216,343,512,729,1000,1331,1728,2197,2744,3375,4096,4913,5832,6859,8000],[1,16,81,256,625,1296,2401,4096,6561,10000,14641,20736,28561,38416,50625,65536,83521,104976,130321,160000],[1,32,243,1024,3125,7776,16807,32768,59049,100000,161051,248832,371293,537824,759375,1048576,1419857,1889568,2476099,3200000],[1,64,729,4096,15625,46656,117649,262144,531441,1000000,1771561,2985984,4826809,7529536,11390625,16777216,24137569,34012224,47045881,64000000],[1,128,2187,16384,78125,279936,823543,2097152,4782969,10000000,19487171,35831808,62748517,105413504,170859375,268435456,410338673,612220032,893871739,1280000000],[1,256,6561,65536,390625,1679616,5764801,16777216,43046721,100000000,214358881,429981696,815730721,1475789056,2562890625,4294967296,6975757441,11019960576,16983563041,25600000000],[1,512,19683,262144,1953125,10077696,40353607,134217728,387420489,1000000000,2357947691,5159780352,10604499373,20661046784,38443359375,68719476736,118587876497,198359290368,322687697779,512000000000],[1,1024,59049,1048576,9765625,60466176,282475249,1073741824,3486784401,10000000000,25937424601,61917364224,137858491849,289254654976,576650390625,1099511627776,2015993900449,3570467226624,6131066257801,10240000000000]]
-}
listaPotencias = [[x^n | x<- [1..20]] | n <- [1..10]]

listaPotencias2 :: [[Int]]
{-|
=Description

List of powers 1 to 10 of each number 1 to 20

==Example
>>>listaPotencias2
[[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],[2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768,65536,131072,262144,524288],[3,9,27,81,243,729,2187,6561,19683,59049,177147,531441,1594323,4782969,14348907,43046721,129140163,387420489,1162261467],[4,16,64,256,1024,4096,16384,65536,262144,1048576,4194304,16777216,67108864,268435456,1073741824,4294967296,17179869184,68719476736,274877906944],[5,25,125,625,3125,15625,78125,390625,1953125,9765625,48828125,244140625,1220703125,6103515625,30517578125,152587890625,762939453125,3814697265625,19073486328125],[6,36,216,1296,7776,46656,279936,1679616,10077696,60466176,362797056,2176782336,13060694016,78364164096,470184984576,2821109907456,16926659444736,101559956668416,609359740010496],[7,49,343,2401,16807,117649,823543,5764801,40353607,282475249,1977326743,13841287201,96889010407,678223072849,4747561509943,33232930569601,232630513987207,1628413597910449,11398895185373143],[8,64,512,4096,32768,262144,2097152,16777216,134217728,1073741824,8589934592,68719476736,549755813888,4398046511104,35184372088832,281474976710656,2251799813685248,18014398509481984,144115188075855872],[9,81,729,6561,59049,531441,4782969,43046721,387420489,3486784401,31381059609,282429536481,2541865828329,22876792454961,205891132094649,1853020188851841,16677181699666569,150094635296999121,1350851717672992089],[10,100,1000,10000,100000,1000000,10000000,100000000,1000000000,10000000000,100000000000,1000000000000,10000000000000,100000000000000,1000000000000000,10000000000000000,100000000000000000,1000000000000000000,-8446744073709551616],[11,121,1331,14641,161051,1771561,19487171,214358881,2357947691,25937424601,285311670611,3138428376721,34522712143931,379749833583241,4177248169415651,45949729863572161,505447028499293771,5559917313492231481,5818858227285891443],[12,144,1728,20736,248832,2985984,35831808,429981696,5159780352,61917364224,743008370688,8916100448256,106993205379072,1283918464548864,15407021574586368,184884258895036416,2218611106740436992,8176589207175692288,5885350117560549376],[13,169,2197,28561,371293,4826809,62748517,815730721,10604499373,137858491849,1792160394037,23298085122481,302875106592253,3937376385699289,51185893014090757,665416609183179841,8650415919381337933,1774942509700083433,4627508552391533013],[14,196,2744,38416,537824,7529536,105413504,1475789056,20661046784,289254654976,4049565169664,56693912375296,793714773254144,11112006825558016,155568095557812224,2177953337809371136,-6402141418087907328,2603740515317055488,-441120932980326400],[15,225,3375,50625,759375,11390625,170859375,2562890625,38443359375,576650390625,8649755859375,129746337890625,1946195068359375,29192926025390625,437893890380859375,6568408355712890625,6292404967145601295,2152354138636261345,-4608176067875183057],[16,256,4096,65536,1048576,16777216,268435456,4294967296,68719476736,1099511627776,17592186044416,281474976710656,4503599627370496,72057594037927936,1152921504606846976,0,0,0,0],[17,289,4913,83521,1419857,24137569,410338673,6975757441,118587876497,2015993900449,34271896307633,582622237229761,9904578032905937,168377826559400929,2862423051509815793,-6679040345461786367,-2863221430593058543,6665467901046659617,2632489875535903793],[18,324,5832,104976,1889568,34012224,612220032,11019960576,198359290368,3570467226624,64268410079232,1156831381426176,20822964865671168,374813367582081024,6746640616477458432,-7687677419372609536,9195759040969441280,-497033925936021504,-8946610666848387072],[19,361,6859,130321,2476099,47045881,893871739,16983563041,322687697779,6131066257801,116490258898219,2213314919066161,42052983462257059,799006685782884121,-3265617043834753317,-6706491611731658175,1703867893065355987,-4519998179177339479,6353754964178307979],[20,400,8000,160000,3200000,64000000,1280000000,25600000000,512000000000,10240000000000,204800000000000,4096000000000000,81920000000000000,1638400000000000000,-4125488147419103232,-8722786653543858176,-8435036407491198976,-2680031486438014976,1739602492368355328]]
-}
listaPotencias2 = [take 19 $ iterate (* i) i | i <-[1..20]]

listaPosNeg :: [Int]
{-|
=Description
List of numbers alternating between positive and negative

==Example
>>>listaPosNeg
[1,-2,3,-4,5,-6,7,-8,9,-10,11,-12,13,-14,15,-16,17,-18,19,-20,21,-22,23,-24,25,-26,27,-28,29,-30,31,-32,33,-34,35,-36,37,-38,39,-40,41,-42,43,-44,45,-46,47,-48,49,-50]
-}
listaPosNeg =take 50 [if odd x then x else negate x | x<- [1..]]

listaPosNeg2 :: [Int]
{-|
=Description
List of numbers alternating between positive and negative of the same value

==Example
>>>listaPosNeg2
[0,1,-1,2,-2,3,-3,4,-4,5,-5,6,-6,7,-7,8,-8,9,-9,10,-10,11,-11,12,-12,13,-13,14,-14,15,-15,16,-16,17,-17,18,-18,19,-19,20,-20,21,-21,22,-22,23,-23,24,-24,25,-25,26,-26,27,-27,28,-28,29,-29,30,-30,31,-31,32,-32,33,-33,34,-34,35,-35,36,-36,37,-37,38,-38,39,-39,40,-40,41,-41,42,-42,43,-43,44,-44,45,-45,46,-46,47,-47,48,-48,49,-49,50]
-}
listaPosNeg2 = take 100 $ 0:[n | x <- [1..], n<-[x,-x]]

listaEnumeracion :: [(Int,Int)]
{-|
=Description
List with every pair of two natural numbers

==Example
>>> take 130 listaEnumeracion
[(0,0),(1,0),(0,1),(2,0),(1,1),(0,2),(3,0),(2,1),(1,2),(0,3),(4,0),(3,1),(2,2),(1,3),(0,4),(5,0),(4,1),(3,2),(2,3),(1,4),(0,5),(6,0),(5,1),(4,2),(3,3),(2,4),(1,5),(0,6),(7,0),(6,1),(5,2),(4,3),(3,4),(2,5),(1,6),(0,7),(8,0),(7,1),(6,2),(5,3),(4,4),(3,5),(2,6),(1,7),(0,8),(9,0),(8,1),(7,2),(6,3),(5,4),(4,5),(3,6),(2,7),(1,8),(0,9),(10,0),(9,1),(8,2),(7,3),(6,4),(5,5),(4,6),(3,7),(2,8),(1,9),(0,10),(11,0),(10,1),(9,2),(8,3),(7,4),(6,5),(5,6),(4,7),(3,8),(2,9),(1,10),(0,11),(12,0),(11,1),(10,2),(9,3),(8,4),(7,5),(6,6),(5,7),(4,8),(3,9),(2,10),(1,11),(0,12),(13,0),(12,1),(11,2),(10,3),(9,4),(8,5),(7,6),(6,7),(5,8),(4,9),(3,10),(2,11),(1,12),(0,13),(14,0),(13,1),(12,2),(11,3),(10,4),(9,5),(8,6),(7,7),(6,8),(5,9),(4,10),(3,11),(2,12),(1,13),(0,14),(15,0),(14,1),(13,2),(12,3),(11,4),(10,5),(9,6),(8,7),(7,8),(6,9)]
-}
listaEnumeracion = [(x-y,y)| x <- [0..], y <- [0..x]]

getPos :: Int -> (Int, Int)
{-|
=Description

Gets the pair of natural numbers at a given position in 'listaEnumeracion'

==Example
>>> getPos 129
(6,9)
-}
getPos = (listaEnumeracion !!)

getPosInv :: (Int, Int) -> Int
{-|
=Description
Given a pair of natural numbers, finds its position in 'listaEnumeracion'

==Example
>>>getPosInv (13,2)
122
-}
getPosInv (n,m) = length $ takeWhile (/=(n,m)) listaEnumeracion


-- =====================================================
-- * EJERCICIO 2
-- =====================================================

filter2 :: [a] -> (a -> Bool) -> (a -> Bool) -> ([a],[a])
filter2 xs p q = (us, vs) where
    us = filter p xs
    vs = filter q xs

filters :: [a] -> [a->Bool]->[[a]]
filters xs ps = [filter p xs |  p <- ps]

span :: (a->Bool) -> [a] -> ([a],[a])
span p xs = (us,vs) where
    us = filter p xs
    vs = filter (not . p) xs

mapx :: Int -> [Int -> a] -> [a]
mapx x fs = mapxAux x fs [] where
    mapxAux :: Int -> [Int->a] -> [a] -> [a]
    mapxAux _ [] acc     = acc
    mapxAux x (f:fs) acc = mapxAux x fs [f x]++acc

iguales :: (Eq a) => (Int -> a) -> (Int -> a) -> Int -> Int -> Bool
iguales f g n m
    | n == m = True
    | otherwise =  (g n == f n) && iguales f g (n+1) m

cuantos :: (a -> Bool) -> [a] -> Int
cuantos p xs = length $ filter p xs

mayoria :: (a->Bool) -> [a] -> Bool
mayoria p xs = cuantos p xs > length xs `div` 2

menorA :: Int -> Int -> (Int -> Bool) -> Int
menorA n m p
    | n > m = error "No match"
    | otherwise = if p n then n else menorA (n+1) m p

menor :: Int -> (Int -> Bool) -> Int
menor n p = last $ takeWhile (not . p) [n..]

mayorA :: Int -> Int -> (Int -> Bool) -> Int
mayorA n m p
    | m < n = error "No match"
    | otherwise = if p m then m else mayorA n (m-1) p

mayor :: Int -> (Int->Bool) -> Int
mayor n p = head $ filter p [n, n-1..]

ex :: Int -> Int -> (Int -> Bool) -> Bool
ex n m p
    | n > m = False
    | otherwise = p n || ex (n+1) m p

pt :: Int -> Int -> (Int -> Bool) -> Bool
pt n m p = all p [n..m]

-- =====================================================
-- * EJERCICIO 3
-- =====================================================

listaDivisores :: Int -> [Int]
listaDivisores x = [d | d<- [1..x], x `mod` d == 0]

listaParejas :: [(Int, [Int])]
listaParejas = [(x, init $ listaDivisores x) | x <- [19..50]]

esPerfecto :: Int -> Bool
esPerfecto x =  x == sum (init $ listaDivisores x)

listaPerfectos :: [Int]
-- >>> listaPerfectos
-- [6,28,496]
listaPerfectos = [x | x <- [1..1000], esPerfecto x]

esPrimo :: Int -> Bool
esPrimo x = length (listaDivisores x) == 2

listadoPrimos :: [Int]
listadoPrimos = [x | x <- [1..], esPrimo x]

listadoPrimosMenores1000 :: [Int]
listadoPrimosMenores1000  = takeWhile(<1000) listadoPrimos

primosEntre200y500 :: [Int]
primosEntre200y500 = takeWhile (<500) $ dropWhile (<200) listadoPrimos

primerPrimoMayorQue6923 :: Int
primerPrimoMayorQue6923 = head $ filter esPrimo [6923..]

esProcesable :: Int -> Bool
-- >>> esProcesable 155
-- True
esProcesable x = suma where
    divisores = drop 1 (takeWhile (<x) $ listaDivisores x)
    divisoresPrimos = length divisores == 2 && all esPrimo divisores
    suma = divisoresPrimos && (x == sum [n | n <- [(head divisores) .. (last divisores)], esPrimo n])

listaProcesables :: [Int]
-- >>> listaProcesables
-- ProgressCancelledException
listaProcesables = take 5 [x | x <- [1..], esProcesable x]


divisoresPrimos :: Int -> [Int]
divisoresPrimos x = [n | n <- listaDivisores x, esPrimo n]

maxExp :: Int -> Int -> Int
maxExp x n = last $ takeWhile (\k -> x `mod` n^k == 0) [0..]


fprimos :: Int -> [(Int, Int)]
-- >>> fprimos 1176
-- [(2,3),(3,1),(7,2)]
fprimos x = [(n,maxExp x n) | n <- divisoresPrimos x]

-- =====================================================
-- * EJERCICIO 4
-- =====================================================

last' :: [a] -> a
last' (x:xs) = foldr (\_ y -> y) x xs

reverse' :: [a] -> [a]
reverse' = foldl (\ys y -> [y]++ys) []

all' :: [a] ->(a->Bool) ->Bool
all' xs p = foldr (\y x -> x && p y) True xs

any' :: [a] ->(a->Bool) ->Bool
any' xs p = foldr (\y x -> x || p y) False xs


concat' :: [[a]] -> [a]
-- >>> concat' [[1,2],[3]]
-- [1,2,3]
concat'  = foldr (++) []

minimum :: (Ord a) => [a] -> a
minimum (x:xs) = foldr min x xs

map' :: [a] -> (a->b) -> [b]
map' xs f = foldr (\x y -> f x : y) [] xs

filter' :: [a] -> (a -> Bool) -> [a]
filter' xs p = foldr (\x ys -> if p x then x:ys else ys) [] xs

takeWhile' ::  [a] -> (a ->Bool) ->[a]
takeWhile' xs p = foldr (\x ys -> if p x then x:ys else []) [] xs


take' :: [a] -> Int -> [a]
take' xs n = foldr (\(idx, x) acc -> if idx < n then x:acc else []) [] $ zip [0..] xs

(+++) :: [a] -> [a]-> [a]
(+++) xs ys = foldr (:) ys xs

-- =====================================================
-- * EJERCICIO 5
-- =====================================================

foldr1' :: (a->a->a) -> [a] -> a
foldr1' f [x]    = x
foldr1' f (x:xs) = f x (foldr1' f xs)


foldl1' :: (a->a->a) -> [a] -> a
foldl1' f (x:xs) = foldl f x xs

-- =====================================================
-- * EJERCICIO 6
-- =====================================================
prefixes :: [a] -> [[a]]
prefixes xs = [take x xs | x <- [0..length xs]]

suffixes :: [a] -> [[a]]
suffixes xs = [drop x xs | x <- [0..length xs]]

sublists :: [a] -> [[a]]
-- [[],[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5],[2],[2,3],[2,3,4],[2,3,4,5],[3],[3,4],[3,4,5],[4],[4,5],[5]]
sublists xs = [] : [zs |ys <- suffixes xs, zs <- prefixes ys, not $ null zs]

partss :: [a] -> [[a]]
partss [] = [[]]
partss xs = foldr (\x yss -> yss ++ [x:ys | ys <- yss]) [[]] xs

perms :: [a] -> [[a]]
perms = foldr (\x y -> [us ++ [x]++ vs | ys <-y,n<-[0..length ys], (us,vs) <- [splitAt n ys]]) []

inits :: [a] -> [[a]]
inits xs = [take n xs | n <- [0 .. length xs]]

combinaciones :: Int -> [a] -> [[a]]
combinaciones 0 _      = [[]]
combinaciones _ []     = []
combinaciones n (x:xs) =combinaciones n xs ++ map (x:) (combinaciones (n-1) xs)

variaciones ::(Eq a) => Int -> [a] -> [[a]]
variaciones 0 _  =[[]]
variaciones _ [] = []
variaciones n xs = [x:ys | x <-xs, ys <- variaciones (n-1) (filter (/=x) xs)]

sumandos :: Int -> [[Int]]
sumandos 0 = [[]]
sumandos 1 = [[1]]
sumandos n = [n] : [1:xs | xs <- sumandos (n-1)]

-- =====================================================
-- * EJERCICIO 7
-- =====================================================

prodEscalar :: [Float] -> [Float] -> Float
prodEscalar xs ys = sum $ zipWith (*) xs ys

numFilas :: [[Float]] -> Int
numFilas m = length m

numColumnas :: [[Float]] -> Int
numColumnas m = length (m !! 0)

fila :: Int -> [[Float]] -> [Float]
fila n m = m !! (n-1)

columna :: Int -> [[Float]] -> [Float]
columna n m = [v !! (n-1) | v <- m]

productoMatricial :: [[Float]] -> [[Float]] -> [[Float]]
productoMatricial m1 m2 = [[prodEscalar (fila i m1) (columna j m2) | j <- [1..numColumnas m2]] | i <- [1..numFilas m1]]

-- =====================================================
-- * EJERCICIO 8
-- =====================================================

f :: Int -> [Int]
f n = map (\x -> x*x) $ filter even [1..n]

g :: Int -> Int -> [Int]
g n m = concat $ map (\x -> map (x +) [x..m]) [1..n]

h:: (Int -> Bool) -> Int -> Int -> [Int]
h p n m = concat $ map (\x -> map (\y -> x + y) [n..m])  $filter (\x -> p (n-x)) [1..n]

-- =====================================================
-- * EJERCICIO 9
-- =====================================================

auxZ :: Int -> [Int] -> Int
auxZ x y = x * last y
auxG :: Int -> Int -> [Int] -> Int
auxG u x y= (x + auxZ x y ) * u
auxL :: Int -> [Int] -> Int -> (Int, Int)
auxL x y u = (auxG u x y, auxG (u + 1) x y)
f9 :: Int -> [Int] -> [(Int, Int)]
f9 x y = map (auxL x y) y

-- =====================================================
-- * EJERCICIO 10
-- =====================================================

collatz :: Integral a => a -> a
collatz n
    | mod n 2 == 0 = div n 2
    | otherwise = 3*n + 1

traza :: Integral a => a -> [a]
-- >>> traza 5
-- [5,16,8,4,2]
traza n = takeWhile (/= 1) (iterate collatz n)

f' :: Integral a => a -> Int
-- >>> f' 5
-- 5
f' n = length (traza n)

f'' :: Integral a => a -> (Int, [a])
-- >>> f'' 5
-- (5,[5,16,8,4,2])
f'' n  = (length m, m) where m = traza n

f''' :: Integral a => a -> [(a, Int)]
-- >>> f''' 5
-- [(1,0),(2,1),(3,7),(4,2),(5,5)]
f''' n = zip [1..n] (map (fst . f'') [1..n])

f'''' :: [(Int, Int)]
f''''  = zip [1..] (map (fst . f'') [1..])

coll1 :: (Int, Int)
-- >>> coll1
-- (703,170)
coll1 = head $ dropWhile (\(n,k) -> k <= 150) f''''

coll2 :: [(Int, Int)]
-- >>> coll2
-- [(1017,155),(1033,155),(1055,168),(1071,168),(1161,181),(1215,163),(1249,176),(1263,176),(1307,176),(1351,158),(1377,158),(1406,171),(1407,171),(1471,171),(1519,153),(1526,153),(1527,153),(1537,153),(1550,153),(1551,153),(1563,153),(1583,166),(1607,166),(1665,179),(1695,179),(1742,179),(1743,179),(1801,161),(1819,161),(1823,161),(1839,161),(1874,174),(1875,174),(1895,174),(1915,174),(1951,174),(1961,174)]
coll2 = filter (\(n,k) -> k > 150) (drop 1000 (f''' 2000))

coll3 :: [(Int, Int)]
coll3 = filter (\(n,k) -> k > n) f''''

-- =====================================================
-- * EJERCICIO 10
-- =====================================================

fix:: (a -> a) -> a
fix f = f (fix f)

fac1 :: (Eq a, Num a) => a -> a
fac1 n = fix facHO n

facHO:: (Eq a, Num a) => (a -> a) -> a -> a
facHO f n = if n==0 then 1 else n*f (n-1)

length1 :: Num p => [a] -> p
length1 xs = fix lengthHO xs

lengthHO :: Num p => ([a] -> p) -> [a] -> p
lengthHO f []     = 0
lengthHO f (x:xs) = 1 + f xs

infixr 5 ++++
(++++) :: [a] -> [a] -> [a]
xs ++++ ys = fix appHO xs ys
appHO :: ([a] -> [a] -> [a]) -> [a] -> [a] -> [a]
appHO f [] ys     = ys
appHO f (x:xs) ys = x:f xs ys
