-- ALUMNOS: ALEJANDRO BARRACHINA ARGUDO
--          CARLOS MURCIA MORILLA

-- 1.a
time :: Integer
time = 10 ^ 6

yearsSeg :: Integer
yearsSeg = 365 * 24 * 3600

daysSeg :: Integer
daysSeg = 24 * 3600

hoursSeg :: Integer
hoursSeg = 3600

minsSeg :: Integer
minsSeg = 60

years :: Integer
years = div time yearsSeg

days :: Integer
days = div (time - (years * yearsSeg)) daysSeg

hours :: Integer
hours = div (time - (years * yearsSeg) - (days * daysSeg)) hoursSeg

mins :: Integer
mins = div (time - (years * yearsSeg) - (days * daysSeg) - (hours * hoursSeg)) minsSeg

segs :: Integer
segs = time - (years * yearsSeg) - (days * daysSeg) - (hours * hoursSeg) - (mins * minsSeg)

sol1a :: (Integer, Integer, Integer, Integer, Integer)
sol1a = (years, days, hours, mins, segs)

-- 1.b

sol1b :: Integer -> (Integer, Integer, Integer, Integer, Integer)
sol1b x =
  let years = div x yearsSeg
      days = div (x - (years * yearsSeg)) daysSeg
      hours = div (x - (years * yearsSeg) - (days * daysSeg)) hoursSeg
      mins = div (x - (years * yearsSeg) - (days * daysSeg) - (hours * hoursSeg)) minsSeg
      segs = (x - (years * yearsSeg) - (days * daysSeg) - (hours * hoursSeg) - (mins * minsSeg))
   in (years, days, hours, mins, segs)

-- 2.a

sol2A :: Integer -> Bool
sol2A year =
  if ((mod year 4 == 0) && (mod year 100 /= 0)) || (mod year 400 == 0) then True else False

-- 2.b
sol2B :: Integer -> Bool
sol2B year
  | mod year 400 == 0 = True
  | (mod year 4 == 0) && (mod year 100 /= 0) = True
  | otherwise = False

-- 3
--Error de tipado ya que la division / no trabaja con Integer
sol3 xs =
  let s = sum xs
      l = length xs
   in fromIntegral s / fromIntegral l

-- 4
calculoDigitos :: Integer -> Integer
calculoDigitos x
  | x < 10 = 1
  | otherwise = 1 + calculoDigitos (div x 10)

sumaDigitos :: Integral t => t -> t
sumaDigitos x
  | x < 10 = x
  | otherwise = mod x 10 + sumaDigitos (div x 10)

reduccion :: Integral t => t -> t
reduccion x
  | x < 10 = x
  | otherwise = let y = sumaDigitos x in if y < 10 then y else reduccion y

-- 5

bools :: Bool -> Bool -> Bool
bools x False = False
bools False x = False
bools True x = x
bools x True = x
