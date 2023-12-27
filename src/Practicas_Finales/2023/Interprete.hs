{-|
Module      : Interprete
Description : Interprete de lenguaje imperativo simple
License     : GPL-3.0
Maintainer  : Alejandro Barrachina Argudo
Stability   : experimental
Portability : unknown

Práctica final de Programación Declarativa (PD) de Ingeniería Informática de la UCM del curso 2023-2024
-}
module Interprete where

-- =========================================
-- * DATA TYPES
-- =========================================

-- | Alias for Variables
type Var = String

-- | Alias for a list of instructions
type Program = [Instruction]

showProgram :: Program -> String

{-|
=Description

Shows a full program
-}
showProgram = foldl (\r p -> r ++ show p ++ "\n") ""

-- | Alias for the program way of handling with variables
type VarVal = (Var, Int)

-- | Alias for the program stack
type State = [VarVal]

mostrarStack :: State -> IO ()

{-|
=Description

Shows the current state of the stack and waits for an enter key press to continue execution
-}
mostrarStack [] = do
  putStrLn "<Enter> para continuar"
  pause <- getLine
  return ()
mostrarStack (s@(var, val) : ss) = do
  putStr (show var ++ " -> " ++ show val ++ "\n")
  mostrarStack ss

-- | Posible arithmetic operations in the program
data Symbol = Plus | Minus | Times | Equals deriving (Read)

instance Show Symbol where
  show Plus   = ":+"
  show Minus  = ":-"
  show Times  = ":*"
  show Equals = ":="

-- | Posible logic operations in the program
data Comparator = Greater | Less | GEQ | LEQ | Eq | Not | And | Or deriving (Read)

instance Show Comparator where
  show Greater = ">"
  show Less    = "<"
  show GEQ     = ">="
  show LEQ     = "<="
  show Eq      = "=="
  show Not     = "!"
  show And     = "&&"
  show Or      = "||"

-- | Arithmetic operations in the program. Can use variables or integers.
data Operation
  = {-| Full operation -}
    O
      Operation
      {-^ variable to change value -}
      Symbol
      {-^ arithmetic operation -}
      Operation
      {-^ Operation to perform -}
  | {-| Operation is only an integer -}
    I
      Int
      {-^ Integer to operate with -}
  | {-| Operation uses a variable -}
    V
      Var
      {-^ Variable to get the value from -}
  deriving (Read)

instance Show Operation where
  show (O op symbol op2) = show op ++ show symbol ++ show op2
  show (I int)           = show int
  show (V var)           = show var

-- | Instructions inside the program, can be assigning new values to variables or while loops
data Instruction
  = {-| Assign operations -}
    A
      Var
      {-^ Variable to change value -}
      Operation
      {-^ Operation to change value -}
  | {-| While loop -}
    W
      Comp
      {-^ Comparator to end the loop -}
      Program
      {-^ Set of instructions inside the loop -}
  deriving (Read)

instance Show Instruction where
  show (A var op) = show var ++ " := " ++ show op
  show (W c ps) = "while (" ++ show c ++ ") {\n" ++ foldl (\r p -> r ++ "\t" ++ show p ++ "\n") "" ps ++ "}"

-- | Boolean comparators for while loops ('W')
data Comp
  = {-| Arithmetic comparator -}
    CO
      Operation
      {-^ Variable to compare -}
      Comparator
      {-^ Comparator used -}
      Operation
      {-^ Integer to compare to -}
  | {-| Boolean Comparator -}
    CB
      Bool
      {-^ First part of the comparison -}
      Comparator
      {-^ Comparator used -}
      Bool
      {-^ Second part of the comparison -}
  | {-| Comparator of comparators -}
    CC
      Comp
      {-^ Variable to compare -}
      Comparator
      {-^ Comparator used -}
      Comp
      {-^ Integer to compare to -}
  | {-| Void comparator, used for not clauses -}
    Void
  deriving (Read)

instance Show Comp where
  show (CO op comp op1) = "(" ++ show op ++ " " ++ show comp ++ " " ++ show op1 ++ ")"
  show (CB b comp b1) = "(" ++ show b ++ " " ++ show comp ++ " " ++ show b1 ++ ")"
  show (CC c comp c1) = "(" ++ show c ++ " " ++ show comp ++ " " ++ show c1 ++ ")"
  show Void = ""

-- =========================================
-- * STACK OPERATIONS
-- =========================================
isVal ::
  -- | current stack of the program
  State ->
  -- | Variable to check
  Var ->
  -- | Variable is in stack or not
  Bool

{-|
=Description

Checks if a given variable name is in the stack.
-}
isVal [] _       = False
isVal (s : ss) v = (fst s == v) || isVal ss v

getVal ::
  -- | Current stack of the program
  State ->
  -- | Variable to check
  Var ->
  -- | Value of the given variable
  Int

{-|
=Description

Gives a the stored value for a value for a variable. If the variable is not in the stack, the function throws an error.
-}
getVal [] _ = error "Variable not in stack"
getVal (s : ss) var
  | fst s == var = snd s
  | otherwise = getVal ss var

setVal ::
  -- | Current stack of the program
  State ->
  -- | Variable to update
  Var ->
  -- | New value
  Int ->
  -- | Updated stack
  State

{-|
=Description

Changes the value of a given variable to a new one.
-}
setVal s var val = map (\x@(f, _) -> if f == var then (f, val) else x) s

-- =========================================
-- * INSTRUCTION OPERATIONS
-- =========================================
resolver :: Comp -> State -> Bool

{-|
=Description

Computes a boolean from the different comparator types
-}
resolver (CO a Greater i) s = calcular a s > calcular i s
resolver (CO a Less i) s = calcular a s < calcular i s
resolver (CO a GEQ i) s = calcular a s >= calcular i s
resolver (CO a LEQ i) s = calcular a s <= calcular i s
resolver (CO a Eq i) s = calcular a s == calcular i s
resolver (CB a Eq b) s = a == b
resolver (CB a And b) s = a && b
resolver (CB a Or b) s = a || b
resolver (CC comp Not Void) s = not $ resolver comp s
resolver (CC comp1 comp comp2) s = resolver (CB (resolver comp1 s) comp (resolver comp2 s)) s

calc ::
  -- | First int
  Int ->
  -- | Operation to perform
  Symbol ->
  -- | Second int
  Int ->
  -- | Result
  Int

{-|
=Description

Performs an arithmetic operation ('Symbol') with two Integers
-}
calc i1 Plus i2  = i1 + i2
calc i1 Minus i2 = i1 - i2
calc i1 Times i2 = i1 * i2

calcular ::
  -- | Operation to perform
  Operation ->
  -- | Current state of the program
  State ->
  -- | Result of the operation
  Int

{-|
=Description

Performs an arithmetic operation given the operation and the current stack of the program
-}
calcular (I int) s        = int
calcular (V var) s        = getVal s var
calcular (O op sym op1) s = calc (calcular op s) sym i where i = calcular op1 s

-- =========================================
-- * EXECUTION
-- =========================================
ejecutarAux ::
  -- | Instruction to execute
  Instruction ->
    -- | Current state of the program
    State ->
    -- | Next state of the program
    State

{-|
=Description

Auxiliary function for 'ejecutar' splitted into the different types of instructions available.
-}
ejecutarAux (W comp p) s =
  if resolver comp s
    then ejecutarAux (W comp p) newState
    else s
  where
    newState = ejecutar p s
ejecutarAux (A var (V var2)) s
  | isVal s var = setVal s var (getVal s var2)
  | otherwise = s ++ [(var, getVal s var2)]
ejecutarAux (A var (I int)) s
  | isVal s var = setVal s var int
  | otherwise = s ++ [(var, int)]
ejecutarAux (A var op) s = setVal s var (calcular op s)

ejecutar ::
  -- | Set of instructions to compute
  Program ->
  -- | Current state of the program
  State ->
  -- | New state of the program
  State

{-|
=Description

Executes all the instructions in a program in sequence
-}
ejecutar ps s = foldl (flip ejecutarAux) s ps -- flip da la vuelta a los argumentos dados

ejecuta ::
  -- \| Set of instructions
  Program ->
  -- | Initial state
  State ->
  -- | Value of the last initialized variable
  Int

{-|
=Description

Executes all the instructions in a given program with a given initial state and returns the last variable declared
-}
ejecuta p s = do getVal newState "R" where newState = ejecutar p s

-- =========================================
-- * IO EXECUTION
-- =========================================
ejecutarAuxIO ::
-- | Instruction to execute
  Instruction ->
  -- | Current state of the program
  State ->
  -- | Next state of the program
  IO State

{-|
=Description

Executes an instruction showing the current state of the stack
-}
ejecutarAuxIO (W comp p) s = do
  if resolver comp s
    then do
      newState <- ejecutarIO p s
      ejecutarAuxIO (W comp p) newState
    else return s
ejecutarAuxIO instruction s = do
  mostrarStack s
  return (ejecutarAux instruction s)

ejecutarIO ::
  -- | Set of instructions
  Program ->
  -- | Initial stack
  State ->
  -- | Value of last initialized variable
  IO State

{- |
=Description

Executes an entire program showing the state of the stack before every instruction. See 'ejecutarAuxIO'
-}
ejecutarIO [] s = return s
ejecutarIO (p : ps) s = do
  newState <- ejecutarAuxIO p s
  ejecutarIO ps newState

ejecutaIO ::
  -- | Set of instructions
  Program ->
  -- | Initial stack
  State ->
  -- | Value of last initialized variable
  IO Int

{-|
=Description

Executes an entire program and returns the value of the R variable showing the state of the stack before every instruction. See 'ejecutarIO'.
-}
ejecutaIO p s = do
  finalState <- ejecutarIO p s
  return (getVal s "R")

-- =========================================
-- * EXAMPLES
-- =========================================
s0 :: State

-- | Initial state for 'factorial'
s0 = [("X", 3)]

s1 :: State

-- | Initial state for 'factorial'
s1 = [("X", 5)]

s2 :: State

-- | Initial state for 'factorial'
s2 = [("X", 20)]

factorial :: Program

{- |
=Description

Factorial calculation for a number given

@
 Y := X
 R := 1
 while (Y > 0) {
  R := R :* Y
  Y := Y :-1
 }
@
-}
factorial =
  [ A "Y" (V "X"),
    A "R" (I 1),
    W
      (CO (V "Y") Greater (I 0))
      [ A "R" (O (V "R") Times (V "Y")),
        A "Y" (O (V "Y") Minus (I 1))
      ]
  ]


s3 :: State

-- | Initial state for 'sumador'
s3 = [("X", 5)]

s4 :: State

-- | Initial state for 'sumador'
s4 = [("X", 7)]

s5 :: State

-- | initial state for 'sumador'
s5 = [("X", 100)]

sumador :: Program

{- |
=Description

Summation of from 1 to a given number

@
i := 1
R := 0
while (i < X && R != __INT_MAX__){
  R := i :+ R
  i := i :+1
}
@
-}
sumador =
  [ A "i" (I 1),
    A "R" (I 0),
    W
      (CC (CO (V "i") LEQ (V "X")) And (CC (CO (V "R") Eq (I (maxBound :: Int))) Not Void)) -- (maxBound :: Int) nos da el valor máximo de Int
      [ A "R" (O (V "i") Plus (V "R")),
        A "i" (O (V "i") Plus (I 1))
      ]
  ]

-- =========================================
-- * EXECUTION MODES
-- =========================================
testResults ::
  -- | debug mode enabled
  Bool ->
  -- | Instructions to execute
  Program ->
  -- | Initial state for the program
  State ->
  -- | Expected result of the program
  Int ->
  IO ()

{-|
=Description

Executes a program with a given state and shows the result compared to the expected result
-}
testResults displayMode p s resEsperado = do
  putStrLn $ showProgram p
  res <- if displayMode then ejecutaIO p s else return (ejecuta p s)
  putStrLn ("\tX = 3 -> Esperado = " ++ show resEsperado ++ "\tObtenido = " ++ show res ++ "\n")

testMode ::
  -- | debug mode enabled
  Bool ->
  IO ()

{-|
=Description

Execution of varios programs with different initial stacks and showing results compared to expected results
-}
testMode displayMode = do
  putStrLn "Valores obtenidos para los distintos test de Factorial:"
  testResults displayMode factorial s0 6
  testResults displayMode factorial s1 120
  testResults displayMode factorial s2 2432902008176640000

  putStrLn "Valores obtenidos para los distintos test de Sumador:"
  testResults displayMode sumador s3 15
  testResults displayMode sumador s4 28
  testResults displayMode sumador s5 5050

freeMode ::
  -- | debug mode enabled
  Bool ->
  IO ()

{-|
=Description

Lets the user write their own program and initial state and shows the result
-}
freeMode displayMode = do
  putStrLn "Introduzca un estado inicial: "
  line <- getLine
  let state = (read line :: State)
  putStrLn "Introduzca un programa en una sola linea: "
  line <- getLine
  let program = (read line :: Program)
  putStrLn $ showProgram program
  res <- if displayMode then ejecutaIO program state else return (ejecuta program state)
  putStrLn ("Resultado = " ++ show res ++ "\n")

fileMode ::
  -- | Debug mode enabled
  Bool ->
  IO ()

{-|
=Description

Lets the user give a file with a written program an then asks for a initial state. Shows results after execution
-}
fileMode displayMode = do
  putStrLn "Introduzca el nombre del fichero a usar: "
  putStr "> "
  file <- getLine
  text <- readFile file
  let program = (read text :: Program)
  putStrLn $ showProgram program
  putStrLn "Introduzca un estado inicial: "
  putStr "> "
  line <- getLine
  let state = (read line :: State)
  res <- if displayMode then ejecutaIO program state else return (ejecuta program state)
  putStrLn ("Resultado = " ++ show res ++ "\n")

-- =========================================
-- * UTILITIES
-- =========================================
getInt :: IO Int

{-|
=Descripción

Coge un número desde la entrada.
-}
getInt = do
  line <- getLine
  return (read line :: Int)

entrypoint :: IO ()

{-|
=Description

Main menu for the program, lets the user choose mode and graphical options
-}
entrypoint = do
  putStrLn "Seleccione un modo de ejecución:\n\t1.-Batería de test\n\t2.-Selección de fichero\n\t3.-Modo libre\n"
  putStr "> "
  let modes = [1, 2, 3]
  selection <- getInt
  putStrLn "Quieres poner el modo debug? (y/n)"
  putStr "> "
  debugMode <- getLine
  let conf = ["y", "n"]
  if debugMode `notElem` conf
    then do
      putStrLn "Opción no reconocida\n"
      entrypoint
    else do
      let debug = debugMode == "y"
      if selection `notElem` modes
        then do
          putStrLn "Selección no valida\n"
          entrypoint
        else if selection == 1 then testMode debug else if selection == 2 then fileMode debug else freeMode debug

main :: IO ()

{-|
=Description

Entrypoint
-}
main = do
  entrypoint
