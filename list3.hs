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
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

-- Merges two ordered lists
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
	| x < y = (x:(merge xs (y:ys)))
	| otherwise = (y:(merge (x:xs) ys))
