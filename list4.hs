-- Merge two sorted lists into one using provided comparison function
merge :: (Ord a) => [a] -> [a] -> (a -> a -> Bool) -> [a]
merge [] [] _ = []
merge xs [] _ = xs
merge [] ys _ = ys
merge (x:xs) (y:ys) cmp
	| cmp x y = x:(merge xs (y:ys) cmp)
	| otherwise = y:(merge (x:xs) ys cmp)

-- Split a list in half
halves :: [a] -> ([a], [a])
halves xs = (take firstHalf xs, reverse (take secondHalf (reverse xs)))
	where
		firstHalf
			| odd len = (div len 2) + 1
			| otherwise = div len 2
			where
				len = length xs
		secondHalf = len - firstHalf
			where len = length xs

-- merge sort with comparison function as input
sort :: (Ord a) => [a] -> (a -> a -> Bool) -> [a]
sort [] _ = []
sort [a] _ = [a]
sort xs cmp = merge (sort left cmp) (sort right cmp) cmp
	where
		left = fst $ halves xs 
		right = snd $ halves xs

-- Creates a list of lists of equal elements grouped together 
group :: (Ord a) => [a] -> [[a]]
group [] = [[]]
group (x:xs)
	| length diffElements == 0 = [x:sameElements]
	| otherwise = [x:sameElements] ++ group diffElements
	where
		sameElements = (filter (\y -> y == x) xs)
		diffElements = (filter (\y -> y /= x) xs)

type Edge = Float
type Triangle = (Edge, Edge, Edge)


-- Functions to get triangle side
side1 :: Triangle -> Float
side1 (s, _, _) = s

side2 :: Triangle -> Float
side2 (_, s, _) = s

side3 :: Triangle -> Float
side3 (_, _, s) = s

-- Function to sum area of all triangles
-- Uses Heron's formula to compute area
sumAreas :: [Triangle] -> Float
sumAreas [] = 0
sumAreas xs = foldr1 (+) areas
	where
		areas = map (\tr -> getArea tr) xs
			where
				getArea tr = sqrt (semiP*(semiP - l1)*(semiP - l2)*(semiP - l3))
					where
						semiP = 0.5*(side1 tr + side2 tr + side3 tr)
						l1 = side1 tr
						l2 = side2 tr
						l3 = side3 tr

-- Functions that receives list of binary functions and list of elements
-- Ands applies each function to each element, returning a list of unary functions
applyFunctions :: [(a -> b -> c)] -> [a] -> [(b -> c)]
applyFunctions [] [] = []
applyFunctions fs [] = []
applyFunctions [] vs = []
applyFunctions (f:fs) (v:vs) = ((f v):(applyFunctions fs vs)) 

-- Solution using words function
__compressAndSortNames :: [String] -> [String]
__compressAndSortNames [] = []
__compressAndSortNames (x:xs) = ( (((head x):". ") ++ (last $ words x)) : __compressAndSortNames(xs) )

_compressAndSortNames xs = sort (__compressAndSortNames xs) (<)


-- Solution without words function

-- Searches for the last index of ' ' (space character)
getLastSpaceIndex :: String -> Int
getLastSpaceIndex xs = foldr (\c idx -> if idx /= -1 then idx else if fst c == ' ' then snd c else -1) (-1) (zip xs [0..])

-- Compress a single name to X. LastName
compressName :: String -> String
compressName xs = ((head xs):(". ")) ++ lastName
	where
		lastSpaceIndex = getLastSpaceIndex xs
		lastNameIndex = filter (\ci -> snd ci > lastSpaceIndex) (zip xs [0..])
		lastName = map (\ci -> fst ci) lastNameIndex

-- Do it!
compressAndSortNames :: [String] -> [String]
compressAndSortNames xs = sort (map (\name -> compressName name) xs) (<)

