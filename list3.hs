-- MDC function using euclidean algorithm
mdc :: Int -> Int -> Int
mdc 0 y = y
mdc x y = (mdc (mod y x) x)

--Checks if prime in sqrt(N) (naive)
_isPrime :: Int -> Int -> Bool
_isPrime _ 0 = False
_isPrime _ 1 = False
_isPrime i n
	| i > floor (sqrt (fromIntegral n)) = True
	| (mod n i) == 0 = False
	| otherwise = (_isPrime (i+1) n)

isPrimeNaive :: Int -> Bool
isPrimeNaive n = _isPrime 2 n

--Checks if prime using eratostenes sieve
buildSieve :: [Int] -> [Int]
buildSieve [] = []
buildSieve (x:l) = (x:buildSieve (filter (\a -> (mod a x /= 0)) l))

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = ((last (buildSieve [2..n])) == n)

