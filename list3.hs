-- MDC function using euclidean algorithm
mdc :: Int -> Int -> Int
mdc 0 y = y
mdc x y = (mdc (mod y x) x)

-- Checks if prime in sqrt(N) (naive)
_isPrime :: Int -> Int -> Bool
_isPrime _ 0 = False
_isPrime _ 1 = False
_isPrime i n
	| i > floor (sqrt (fromIntegral n)) = True
	| (mod n i) == 0 = False
	| otherwise = (_isPrime (i+1) n)

isPrimeNaive :: Int -> Bool
isPrimeNaive n = _isPrime 2 n

-- Checks if prime using eratostenes sieve
buildSieve :: [Int] -> [Int]
buildSieve [] = []
buildSieve (x:l) = (x:buildSieve (filter (\a -> (mod a x /= 0)) l))

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = ((last (buildSieve [2..n])) == n)

-- Computes distance from two 3D points
type Point3D = (Double, Double, Double)

xPoint3D :: Point3D -> Double
xPoint3D (x, _, _) = x

yPoint3D :: Point3D -> Double
yPoint3D (_, y, _) = y

zPoint3D :: Point3D -> Double
zPoint3D (_, _, z) = z

distance :: Point3D -> Point3D -> Double
distance p1 p2 = sqrt (xDiff*xDiff + yDiff*yDiff + zDiff*zDiff)
	where
		xDiff = (xPoint3D p1)-(xPoint3D p2)
		yDiff = (yPoint3D p1)-(yPoint3D p2)
		zDiff = (zPoint3D p1)-(zPoint3D p2)

-- Returns sum of square of first 100 positive integers
sum100FirstsSquares :: Integer
sum100FirstsSquares = sum [x*x | x <- [1..100]]

-- Returns all pairs (x,y) for x in [0..m] and y in [0..y] 
grid :: (Integral a) => a -> a -> [(a, a)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

-- Merges two ordered lists
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
	| x < y = (x:(merge xs (y:ys)))
	| otherwise = (y:(merge (x:xs) ys))

-- Splits list into two (in the half)
halves :: (Ord a) => [a] -> ([a], [a])
halves xs = (take firstHalf xs, reverse (take secondHalf (reverse xs)))
	where
		firstHalf
			| odd $ len = (len `div` 2 ) + 1
			| otherwise = div len 2
			where
				len = length xs
		secondHalf = len - firstHalf
			where
				len = length xs

-- MergeSort implementation 
msort :: (Ord a) => [a] -> [a]
msort [] = []
msort [b] = [b]
msort xs = merge (msort left) (msort right)
	where
		left = fst $ halves xs
		right  = snd $ halves xs

-- Receives list of functions and list of values and apply each value to each function
applyUnaryFunctions :: [(a -> b)] -> [a] -> [b]
applyUnaryFunctions [] [] = []
applyUnaryFunctions [] vs = []
applyUnaryFunctions fs [] = []
applyUnaryFunctions (f:fs) (v:vs) = (f v):(applyUnaryFunctions fs vs) 

-- Receives list of functions and list of values and apply each value to each function and returns unary function
applyBiFunctions :: [(a -> b -> c)] -> [a] -> [(b -> c)]
applyBiFunctions [] [] = []
applyBiFunctions [] vs = []
applyBiFunctions fs [] = []
applyBiFunctions (f:fs) (v:vs) = (f v):(applyBiFunctions fs vs) 

data WeekDay = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday

instance Show WeekDay where
	show (Sunday) = "Sunday"
	show (Monday) = "Monday"
	show (Tuesday) = "Tuesday"
	show (Wednesday) = "Wednesday"
	show (Thursday) = "Thursday"
	show (Friday) = "Friday"
	show (Saturday) = "Saturday"

instance Enum WeekDay where
	fromEnum Sunday = 0
	fromEnum Monday = 1
	fromEnum Tuesday = 2
	fromEnum Wednesday = 3
	fromEnum Thursday = 4
	fromEnum Friday = 5
	fromEnum Saturday = 6

	toEnum 0 = Sunday
	toEnum 1 = Monday
	toEnum 2 = Tuesday
	toEnum 3 = Wednesday
	toEnum 4 = Thursday
	toEnum 5 = Friday
	toEnum 6 = Saturday

instance Ord WeekDay where
	a <= b = fromEnum a < fromEnum b

-- This way, we don't have to write every pair of week day
instance Eq WeekDay where
	(a) == (b) = fromEnum a == fromEnum b

-- Sort weekdays
sortWeekDays :: [WeekDay] -> [WeekDay]
sortWeekDays [] = []
sortWeekDays xs = msort xs

-- (Weekday, date) Weekday and returns all dates that are the same weekday as the second paremeter
sameWeekDay :: [(WeekDay, Int)] -> WeekDay -> [Int]
sameWeekDay [] _ = []
sameWeekDay tps wkd = [snd tp | tp <- tps, fst tp == wkd]

-- Receives the first WeekDay of the month and returns all the days
printMonth :: WeekDay -> [(WeekDay, Int)]
printMonth firstDay = [((toEnum (mod (offset+day-1) 7)::WeekDay), day) | day <- [1..31]]
	where
		offset = fromEnum firstDay

