-- Returns a list with adjacents equal elements (dropping one of them)
groupAdjacents :: [Int] -> [Int]
groupAdjacents [] = []
groupAdjacents (a:[]) = []
groupAdjacents (a:(b:xs)) 
	| a == b = (b:(groupAdjacents (b:xs)))
	| otherwise = groupAdjacents (b:xs)

-- Same as above function, but using list comprehension
-- First generates list with (Ai, i+1) for each element Ai *(1)
-- Takes current element if the next is equal *(2)
groupAdjacentsListComp :: [Int] -> [Int]
groupAdjacentsListComp xs = [ fst t  | 
		t <- zip xs [1..], -- *(1)
		(snd t < length xs) && (fst t == ( xs !! (snd t) ) ) -- *(2)
	]

-- Auxiliar function to trim odd elements between [10,100]
mapFilterFunc :: Int -> Bool
mapFilterFunc el
	| el >= 10 && el <= 100 && mod el 2 /= 0 = False
	| otherwise = True

negatedMapFilterFunc x = not (mapFilterFunc x)

-- Checks if there are odd elements between [10,100] using map
trimOddsInRangeMap :: [Int] -> Bool
trimOddsInRangeMap xs = (length ((filter (\x -> not x) (map mapFilterFunc xs))) == 0)

-- Checks if there are odd elements between [10,100] using filter
trimOddsInRangeFilter :: [Int] -> Bool
trimOddsInRangeFilter xs = not (length (filter negatedMapFilterFunc xs) > 0)

-- Checks if there are odd elements between [10,100] using foldr
trimOddsInRangeFoldr :: [Int] -> Bool
trimOddsInRangeFoldr xs = (foldr (\x y -> if mapFilterFunc x then y else 1) 0 xs) == 0

type Vendor = String
type Power  = Double

data Light = Compact Vendor Power | Incandescent Vendor Power

-- Overrides show with own implementation
instance Show Light where
	show (Compact _ _) = "Compact"
	show (Incandescent _ _) = "Incandescent"

-- Overloads operator (==) and (/=)
instance Eq Light where
	(Compact a b) == (Compact c d) = (a == c && b == d)
	(Incandescent a b) == (Incandescent c d) = (a == c && b == d)
	_ == _ = False

main = do
	putStrLn( show (groupAdjacents [1,2,2,2,3,3,1]))
	
	putStrLn( show (trimOddsInRangeMap [1,26,153,72,68,9]))
	putStrLn( show (trimOddsInRangeMap [1,12,153,73,9]))
	putStrLn( show (trimOddsInRangeMap []))
	putStrLn( show (trimOddsInRangeMap [1,255]))

	putStrLn( show (trimOddsInRangeFilter [1,26,153,72,68,9]))
	putStrLn( show (trimOddsInRangeFilter [1,12,153,73,9]))
	putStrLn( show (trimOddsInRangeFilter []))
	putStrLn( show (trimOddsInRangeFilter [1,255]))

	putStrLn( show (trimOddsInRangeFoldr [1,26,153,72,68,9]))
	putStrLn( show (trimOddsInRangeFoldr [1,12,153,73,9]))
	putStrLn( show (trimOddsInRangeFoldr []))
	putStrLn( show (trimOddsInRangeFoldr [1,255]))

