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

getPower (Compact _ power) = power
getPower (Incandescent _ power) = power

-- Overrides show with own implementation
instance Show Light where
	show (Compact _ _) = "Compact"
	show (Incandescent _ _) = "Incandescent"

-- Overloads operator (==) and (/=)
instance Eq Light where
	(Compact a b) == (Compact c d) = (a == c && b == d)
	(Incandescent a b) == (Incandescent c d) = (a == c && b == d)
	_ == _ = False

data Luster = Luster Luster Luster | LusterLight Light -- YOLO

-- Gets first luster
fstLuster :: Luster -> Luster
fstLuster (Luster l1 _) = l1

-- Gets second luster
sndLuster :: Luster -> Luster
sndLuster (Luster _ l2) = l2

-- Gets light from luster leaf
light :: Luster -> Light
light (LusterLight l) = l

-- leaves
a = LusterLight (Compact "l1" 125) 
b = LusterLight (Incandescent "l2" 12.5) 
c = LusterLight (Compact "l3" 12.5) 
d = LusterLight (Incandescent "l4" 125)

-- Parents Lusters (Barss)
p1 = Luster a d
p2 = Luster b c
p3 = Luster p1 p2

-- Computes total power from a luster
totalPower :: Luster -> Double
totalPower (LusterLight l) = getPower l
totalPower (Luster l1 l2) = totalPower l1 + totalPower l2

-- Checks if luster is balanced
-- LusterLight are balanced!
-- Luster with 2 LusterLight is balanced if power of lights are the same
-- Otherwise -> Check if both Lusters are balanced
lusterBalanced :: Luster -> Bool
lusterBalanced (LusterLight _) = True
lusterBalanced (Luster (LusterLight l1) (LusterLight l2)) = getPower l1 == getPower l2
lusterBalanced (Luster l1 l2) = (lusterBalanced l1) && (lusterBalanced l2)

main = do
	putStrLn( show (groupAdjacents [1,2,2,2,3,3,1]))
	putStrLn( show (groupAdjacentsListComp [1,2,2,2,3,3,1]))
	
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

