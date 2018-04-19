-- Returns a list with adjacents equal elements (dropping one of them)
f1 :: [Int] -> [Int]
f1 [] = []
f1 (a:[]) = []
f1 (a:(b:xs)) 
	| a == b = (b:(f1 (b:xs)))
	| otherwise = f1 (b:xs)

main = do
	putStrLn( show (f1 [1,2,2,2,3,3,1])) 