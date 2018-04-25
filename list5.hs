import Fs

-- Returns all factorials from 1..n using list comprehesion
factorials :: Integer -> [Integer]
factorials n = [foldl1 (\x ac -> x*ac) (filter (\y -> y <= curN) listElems) | curN <- listElems]
	where
		listElems = [1..n]

-- Different implementantion for
-- function that receives a predicate function and a list
-- and test if every element of the list receives True verdict 

testList :: (a -> Bool) -> [a] -> Bool
testList _ [] = True 
testList f (x:xs) = (f x) && (testList f xs)

testListMap :: (a -> Bool) -> [a] -> Bool
testListMap f xs = and (map (\x -> f x) xs)

testListFoldr :: (a -> Bool) -> [a] -> Bool
testListFoldr f xs = foldr (\x ac -> (f x) && ac) True xs

-- Filesystem related solutions 

-- Returns name of a file (Folder or SimpleFile)
fileName :: File -> String
fileName (SimpleFile name _) = name
fileName (Folder name _) = name

-- Returns content of a SimpleFile
simpleFileContent :: File -> String
simpleFileContent (SimpleFile _ content) = content
simpleFileContent _ = error "Parameter is not a simple file!"

-- Returns all files inside a Folder
folderContent :: File -> [File]
folderContent (Folder _ files) = files
folderContent _ = error "Parameter is not a folder!"

-- Checks if a file is a Folder
isFolder :: File -> Bool
isFolder (Folder _ _) = True
isFolder _ = False

-- Checks if a file is a SimpleFile
isSimpleFile :: File -> Bool
isSimpleFile file = not $ isFolder file

instance Eq File where
	(SimpleFile name1 content1) == (SimpleFile name2 content2) = (name1 == name2) && (content1 == content2)
	(Folder name1 fileList1) == (Folder name2 fileList2) = (name1 == name2) && (fileList1 == fileList2)
	_ == _ = False

instance Show File where
	show (SimpleFile name _) = name
	show (Folder name _) = name

-- ^.- -.^ --
changeDir :: File -> String -> String
changeDir (Folder _ files) dirName
	| null targetFiles = error "There is no such a directory!"
	| otherwise = fileName $ head targetFiles 
		where
			targetFiles = filter (\x -> isFolder x) [file | file <- files, fileName file == dirName]

-- Unpack list of lists into a single list
-- (Implementation of Prelude.concat)
unpack :: [[a]] -> [a]
unpack [] = []
unpack ([]:xs) = unpack xs
unpack ((x:xs):xss) = (x:(unpack (xs:xss)))

-- Recursively find name of simple files for a folder
findFiles :: File -> [String]
findFiles (Folder fname fcontent) = [unpack $ findFiles file | file <- fcontent]
findFiles (SimpleFile fname fcontent) = [fname]

-- From a Filesystem, find all SimpleFile names
allSimpleFiles :: FileSystem -> [String]
allSimpleFiles files = filter (\x -> not $ null x) (unpack [findFiles f | f <- files])

-- Creates a folder in a FileSystem, if a file with its name doesn't exists yet
createFolder :: FileSystem -> String -> FileSystem
createFolder fs folderName 
	| fileExists = fs
	| otherwise = ((Folder folderName []):fs)
		where
			fileExists = not $ null (filter (\file -> (fileName file) == folderName) fs)

main = do
	putStrLn "Compiles"
