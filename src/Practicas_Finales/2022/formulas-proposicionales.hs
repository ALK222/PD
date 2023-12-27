{-|
Module      : FormulasProposicionales
Description : Sistema de resolución de formulas proposicionales simple
License     : GPL-3.0
Maintainer  : Alejandro Barrachina Argudo
Stability   : experimental
Portability : unknown

Práctica final de Programación Declarativa (PD) de Ingeniería Informática de la UCM del curso 2022-2023
-}
module FormulasProposicionales
(
    -- * Types
    Prop,
    Formula,
    Clausula,

    -- * Functions
    -- ** Parsers
    fncAlista,
    clausulaLista,
    formulaAfnc,
    listaAtupla,

    -- ** Utilities
    esPositivo,
    esClausula,
    borrarLista,
    indice,

    -- ** Operations on Formulas
    esClausulaHorn,
    resolvente,
    deMorgan,
    distributiva,
    resolucion,

    -- ** IO
    getFormula,
    getInt,
    menu,
    demoMode

)
where

-- ===========================
--       Primera parte
-- ===========================

-- | Atomic predicate
data Prop = Neg String -- ^ Negative predicate
    | Pos String  -- ^ Positive predicate
    deriving (Eq, Show, Read)

-- | Formula constructor
data Formula = Atom String -- ^ Atomic proposition
    | Or Formula Formula -- ^ Formula v Formula
    | And Formula Formula -- ^ Formula ^ Formula
    | Not Formula -- ^ ! Formula
    deriving (Eq, Read)

-- | Alias for list os propositions as clause
type Clausula = [Prop]
instance Show Formula where
    show (Or a b)  = "(" ++ show a ++ "v" ++ show b ++ ")"
    show (And a b) = "(" ++ show a ++ "^" ++ show b ++ ")"
    show (Not a)   = "!" ++ show a
    show (Atom a)  = show a

esPositivo :: Prop -> Bool
{-|
__Description__

Checks if a proposition is positive or negative

=== __Example__

>>> esPositivo (Pos "q")
True

>>> esPositivo (Neg "q")
False
-}
esPositivo (Pos a) = True
esPositivo (Neg a) = False

esClausula :: Formula -> Bool
{- |
__Description__

Checks if a formula can be converted to a clause

=== __Example__

>>> esClausula (Or (Atom "A") (And (Atom "B") (Atom "C")))
False

>>> esClausula (Or (Not (Atom "A")) (Atom "B"))
True
-}
esClausula (Or a b) = esClausula a && esClausula b
esClausula (Atom a) = True
esClausula (Not a)  = esClausula a
esClausula _        = False


fncAlista :: Formula -> [Clausula]
{-|

__Description__

Translates a CNF function to a list of clauses

=== __Example__

>>> fncAlista (And (Or (Atom "A") (Atom "B")) (Not (Atom "B")))
[[Pos "A",Pos "B"],[Neg "B"]]
-}
fncAlista (And a b) = fncAlista a ++ fncAlista b
fncAlista f         = [clausulaLista f]

clausulaLista :: Formula -> Clausula
{-|
__Description__

Translates a Formula wich without (^) to a list of proposition

=== __Example__

>>> clausulaLista (Not (Atom "q"))
[Neg "q"]

>>> clausulaLista (Or (Not (Atom "Q")) (Atom "R"))
[Neg "Q",Pos "R"]
-}
clausulaLista (Or a b)       = clausulaLista a  ++ clausulaLista b
clausulaLista (Atom a)       = [Pos a]
clausulaLista (Not (Atom a)) = [Neg a]
clausulaLista (Not a)        = clausulaLista a

esClausulaHorn :: Clausula -> Bool
{- |

__Description__

Checks if a given clause is a Horn's clause (one or zero positive propositions)

=== __Example__

>>> esClausulaHorn [Neg "A", Pos "B", Pos "C"]
False

>>> esClausulaHorn [Neg "A"]
True

>>> esClausulaHorn [Neg "A", Neg "B", Pos "C"]
True
-}
esClausulaHorn f = length (filter esPositivo f) <= 1

resolvente :: Clausula -> Clausula -> Clausula
{-|

__Description__

Given a 'Clausula' gives the resolvents with another 'Clausula'

=== __Example__

>>> resolvente (clausulaLista (Or (Atom "P") (Or (Not (Atom "Q")) (Atom "R")))) (clausulaLista (Or (Atom "P") (Or (Not (Atom "R")) (Atom "S"))))
[Neg "Q",Pos "P",Pos "S"]


-}
resolvente [] xs = xs
resolvente (Neg x:xs1) xs2
    | Pos x `elem` xs2 = resolvente xs1 (borrar (Pos x) xs2)
    | Neg x `elem` xs2 = resolvente xs1 xs2
    | otherwise = Neg x:resolvente xs1 xs2
resolvente (Pos x: xs1) xs2
    | Neg x `elem` xs2 = resolvente xs1 (borrar (Neg x) xs2)
    | Pos x `elem` xs2 = resolvente xs1 xs2
    | otherwise = Pos x:resolvente xs1 xs2

borrar :: Prop -> Clausula -> Clausula
{-|
= Description

Aux function for 'resolvente' that erases a 'Prop' from a 'Clausula' if found

=== __Example__

>>> borrar (Pos "A") [Pos "B", Neg "A", Neg "C"]
[Pos "B",Neg "A",Neg "C"]

>>> borrar (Pos "A") [Pos "B", Pos "A", Neg "C"]
[Pos "B",Neg "C"]
-}
borrar _ [] = []
borrar (Pos a) ((Pos x):xs) = if x == a then borrar (Pos a) xs else Pos x:borrar (Pos a) xs
borrar (Neg a) ((Neg x):xs) = if x == a then borrar (Neg a) xs else Neg x:borrar (Neg a) xs
borrar a (x:xs) = x:borrar a xs
-- ===========================
--       Segunda parte
-- ===========================

deMorgan :: Formula -> Formula
{-|
__Description__

Applies deMorgan properties to a 'Formula'

=== __Example__

>>> deMorgan (Not (And (Atom "A") (Atom "B")))
(!"A"v!"B")

>>> deMorgan (Not (Or (Atom "A") (Atom "B")))
(!"A"^!"B")

>>> deMorgan (Not (Or (Atom "A") (Not (Atom "B"))))
(!"A"^"B")

>>> deMorgan (Not (And ((Not (Or (Atom "A") (Atom "B")))) (Atom "B")))
(("A"v"B")v!"B")
-}
deMorgan (Not (And a b)) = deMorgan (Or (Not a) (Not b))
deMorgan (Not (Or a b))  = deMorgan (And (Not a) (Not b))
deMorgan (Not (Not a))   = deMorgan a
deMorgan (Not (Atom a))  = Not (Atom a)
deMorgan (Or a b)        = Or (deMorgan a) (deMorgan b)
deMorgan (And a b)       = And (deMorgan a) (deMorgan b)
deMorgan (Atom a)        = Atom a

distributiva :: Formula -> Formula
{-|

__Description__

Applies distributed properties to a 'Formula' but only to transform to NCF, not complete distributed.

=== __Example__
>>> distributiva (And ((Or (Atom "A") (Atom "B"))) (Atom "B"))
(("A"^"B")v("B"^"B"))

>>> distributiva (Not (Or (And (Atom "A") (Atom "B")) (Atom "C")))
!(("A"v"C")^("B"v"C"))

-}
distributiva (Or (And a b) c) = And (distributiva (Or a c)) (distributiva (Or b c))
distributiva (Or a (And b c)) = And (distributiva (Or a b)) (distributiva (Or a c))
-- distributiva (And (Or a b) c) = Or (distributiva (And a c)) (distributiva (And b c))
-- distributiva (And a (Or b c)) = Or (distributiva (And a b)) (distributiva (And a c))
distributiva (And a b)        = And (distributiva a) (distributiva b)
distributiva (Or a b)         = Or (distributiva a) (distributiva b)
distributiva (Not f)          = Not (distributiva f)
distributiva f                = f



formulaAfnc :: Formula -> Formula
{-|

__Description__
Applies 'distributiva' and 'deMorgan' to a 'Formula' to change it to its NCF

=== __Example__
>>> formulaAfnc (And (Not (And (Atom "A") (Atom "B"))) (Atom "C"))
((!"A"^"C")v(!"B"^"C"))
-}
formulaAfnc = distributiva . deMorgan


borrarLista:: [Clausula] -> Clausula -> [Clausula]
{-|
__Description__
Aux function for 'resolucion' that erases a Clause that is no longer needed

=== __Example__
>>> borrarLista [[Neg "Q", Neg "S"], [Pos "S", Neg "Q"]] [Pos "S", Neg "Q"]
[[Neg "Q",Neg "S"]]
-}
borrarLista [] _ = []
borrarLista (x: xs) y
    | x == y = borrarLista xs y
    | otherwise = x:borrarLista xs y

indice :: [Clausula] -> Clausula -> Int -> Int
{-|
 __Description__

Aux function for 'resolucion' that gets the index of a given clause

=== __Example__
>>> indice [[Neg "Q", Neg "S"], [Pos "S", Neg "Q"]] [Pos "S", Neg "Q"] 0
1
-}
indice [] _ _ = -1
indice (x:xs) y i
    | x == y = i
    | otherwise = indice xs y (i+1)

listaAtupla :: [Clausula] -> [Clausula] -> [(Clausula, Clausula)]
{-|
__Description__

Aux function for 'resolucion' that adds two list of lists to a list of tuples of each list

=== __Examples__
>>> listaAtupla [[Neg "A"], [Pos "A", Neg "B"]] [[Neg "A"], []]
[([Neg "A"],[Neg "A"]),([Pos "A",Neg "B"],[])]
-}
listaAtupla [] []         = []
listaAtupla (x:xs) (y:ys) = (x,y) : listaAtupla xs ys

resolucion :: [Clausula] -> Clausula -> IO Clausula
{-|
__Descripcion__

Makes resolvants for a function until no more resolvants are posible

-}
resolucion _ []       = return []
resolucion xs x = do
    let c = map (`resolvente` x) xs
    let a = [x |
           xss <- listaAtupla xs c,
           length (fst xss) /= length (snd xss),
           x <- [snd xss]]

    let aux = filter (not . null) a
    if null aux then do
        putStrLn "No se encontraron más resolventes"
        return []
    else
        do
            let i = indice c (head aux) 0
            let new = borrarLista xs (xs !! i)
            -- print (head aux)
            print "Nuevo resolvente:"
            print (head aux)
            sigRes <- resolucion new (head aux)
            if null sigRes then do return (head aux) else do return sigRes


-- ===========================
--       Tercera parte
-- ===========================

getFormula:: IO Formula
{-|
__Description__

Reads a 'Formula' from the standard input
-}
getFormula = do
    line <- getLine
    return (read line :: Formula)

getInt :: IO Int
{-|
Reads an int from the standard input
-}
getInt = do
    line <- getLine
    return (read line :: Int)

menu :: Formula -> Formula -> IO ()
{-|
__Description__

Simple menu to test functions
-}
menu formula ref = do
    print formula
    print ref
    print "Opciones:"
    print "    1.-Resolver la fórmula"
    print "    2.-Pasarla a FNC"
    print "    3.-Introducir otra formula"
    print "Salir del programa con cualquier otro número"

    option <- getInt

    if option == 1 then
        do
            let fnc = formulaAfnc formula
            let l = fncAlista fnc
            let r = clausulaLista ref
            res <- resolucion l r
            print "Ultimo resolvente encontrado:"
            print res
            menu formula ref

    else
        if option == 2 then
            do
            print (formulaAfnc formula)
            menu formula ref
        else
            if (option == 3) then
                do
                    demoMode
            else
                return ()

demoMode :: IO ()
{-|
__Description__

Demo for the program

Try:
f: (And (And (Or(Atom "A")(Not (Atom "B")))(Or (Not (Atom "A")) (Atom "B"))) (Atom "A"))
r: (Not (Atom "B"))

-}
demoMode = do
    print "Introduzca la formula en expresiones de tipo Formula (todas sus clausulas deben de ser Clausulas de Horn):"
    formula <- getFormula
    if all esClausulaHorn (fncAlista formula)
        then
            do
            print "introduce la refutación:"
            ref <- getFormula
            menu formula ref
        else
            do
            print "Formula no valida: todas las clausulas deben de ser clausulas de Horn\n"
            demoMode
