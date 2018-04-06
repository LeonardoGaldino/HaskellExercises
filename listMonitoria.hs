
--Returns the smallest element in a list
leastElement :: [Int] -> Int
leastElement [] = error "Empty list can't have a smallest element!"
leastElement (x:[]) = x
leastElement (x:xs) 
	| x < subList = x
	| otherwise = subList
	where subList = leastElement xs

--Map function for two lists
doubleMap :: (a -> b -> b) -> [a] -> [b] -> [b]
doubleMap f [] [] = []
doubleMap f (x:xs) (y:ys) = if length xs /= length ys
						then error "Listas de tamanhos diferentes!"
						else ((f x y):(doubleMap f xs ys))

--Fibonacci recursion function using PD technique (Linear complexity)
_fibonacciLinear :: Integer -> [Integer] -> Integer
_fibonacciLinear 0 [a, b] = b
_fibonacciLinear n [a, b] = _fibonacciLinear (n-1) [a+b, a]

--Fibonacci main function using PD technique (Linear complexity)
fibonacciLinear :: Integer -> Integer
fibonacciLinear n = _fibonacciLinear n [1, 0]

--Auxiliar function to merge lists (used in allSequences function)
mergeLists :: [[[Int]]] -> [[Int]]
mergeLists [] = []
mergeLists ([]:xss) = mergeLists xss
mergeLists ((x:xs):xss) = (x:(mergeLists (xs:xss)))

--Auxiliar function to returns all sublists (sequencial) of size N
sequencesLengthN :: Int -> [Int] -> [[Int]]
sequencesLengthN 0 _ = []
sequencesLengthN n xs
	| n > length xs = []
	| otherwise = ((take n xs):(sequencesLengthN n (tail xs)))

--Returns all subsequences of a list
allSequences :: [Int] -> [[Int]]
allSequences xs = ([]: (mergeLists [sequencesLengthN len xs | len <- [0..(length xs)]]) )

--FALSE haskell quicksort
--True quicksort use mutability to do things in-place, hence it's faster
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = (quicksort smallerEls ++ (x:(quicksort greaterEqEls)))
	where 
		smallerEls = filter (\y -> y < x) xs
		greaterEqEls = filter (\y -> y >= x) xs

--Main function to test above functions
main = do
	putStrLn(show(leastElement [5,6,12,-32,4]))
	putStrLn(show(doubleMap (\x y -> x*y) [5,6,12] [-32.1, 7.1, 4.1]))
	putStrLn(show(fibonacciLinear 31))
	putStrLn(show(quicksort [15,15,15,15,15,14,14,14]))
	putStrLn(show(allSequences [1,2,3,4]))