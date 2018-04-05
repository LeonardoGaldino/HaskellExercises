import Data.Char

--Traditional bitwise XOR implementation
xor :: Bool -> Bool -> Bool
xor b1 b2 = not (b1 && b2) && (b1 || b2)

--Bitwise XOR implementation using pattern matching
xor2 :: Bool -> Bool -> Bool
xor2 True True = False
xor2 False False = False
xor2 _ _ = True

--Negated bitwise AND implementation
nAnd :: Bool -> Bool -> Bool
nAnd b1 b2 = not (b1 && b2)

--Negated bitwise AND implementation using pattern matching
nAnd2 :: Bool -> Bool -> Bool
nAnd2 True True = False
nAnd2 _ _ = True

--Implementation of toUpper using list comprehension
strToUpper :: String -> String
strToUpper str = [if ch >= 'a' && ch <= 'z' then chr(ord 'A' + ord ch - ord 'a') else ch | ch <- str]

--Implementation of toUpper using recursive list deconstruction and guards
strToUpper2 :: String -> String
strToUpper2 [] = []
strToUpper2 (ch:str)
	| (ch >= 'a' && ch <= 'z') = ( (chr (ord 'A' + ord ch - ord 'a')) : strToUpper2 str )
	| otherwise = ( ch:strToUpper2 str )

--Receives an character from ['0' - '9'] and returns it's respective digit as integer
--Throws exception if input is not a digit
charToNum :: Char -> Int
charToNum ch  
	| ch >= '0' && ch <= '9' = (ord ch - ord '0')
	| otherwise = error ("Input character is not a digit! Input: " ++ [ch])

--Main function to test functions
main = do 
	putStrLn (show (xor True True))
	putStrLn (show (xor2 True True))
	putStrLn (show (nAnd True True))
	putStrLn (show (nAnd2 True True))
	putStrLn (strToUpper "Haskell is fucked up!")
	putStrLn (strToUpper2 "Haskell is totally fucked up!")
	putStrLn (show (charToNum '3'))
