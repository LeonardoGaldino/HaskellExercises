--Doubles each element of the input list
double :: [Int] -> [Int]
double xs = [2*x | x <- xs]

--Checks if an element belongs to a List
belongsTo :: [Int] -> Int -> Bool
belongsTo [] x = False
belongsTo (y:xs) x = ( (x == y) || (belongsTo xs x) )

belongsTo2 :: [Int] -> Int -> Bool
belongsTo2 xs x = (length [val | val <- xs, val == x]) > 0

--Returns a only digits string from the input string
digits :: String -> String
digits cs = [c | c <- cs, c >= '0' && c <= '9']

--Returns the sum of each pair of the input list
sumPairs :: [(Int, Int)] -> [Int]
sumPairs [] = []
sumPairs (x:xs) = (fst x + snd x):sumPairs xs

type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa, Livro)]

baseExemplo :: BancoDados
baseExemplo = [("Joao","Software Abstractions"), ("Andre","Programming in Haskell"), 
				("Fernando","Introduction to Programming with Python"), ("Fernando","Programming in Haskell")]

--Returns a list of books from a person
livros :: BancoDados -> Pessoa -> [Livro]
livros xs pessoaAlvo = [snd x | x <- xs, fst x == pessoaAlvo]

livros2 :: BancoDados -> Pessoa -> [Livro]
livros2 [] _ = []
livros2 ((pessoa, livro):xs) pessoaAlvo
	| pessoa == pessoaAlvo = livro:(livros2 xs pessoaAlvo)
	| otherwise = (livros2 xs pessoaAlvo)

--Returns a list of persons who have the book
emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos xs livroAlvo = [fst x | x <- xs, snd x == livroAlvo]

emprestimos2 :: BancoDados -> Livro -> [Pessoa]
emprestimos2 [] _ = []
emprestimos2 ((pessoa, livro):xs) livroAlvo
	| livro == livroAlvo = pessoa:(emprestimos2 xs livroAlvo)
	| otherwise = emprestimos2 xs livroAlvo

--Checks if a book is rented
emprestado :: BancoDados -> Livro -> Bool
emprestado xs livroAlvo = (length [x | x <- xs, snd x == livroAlvo]) > 0

emprestado2 :: BancoDados -> Livro -> Bool
emprestado2 [] _ = False
emprestado2 ((pessoa, livro):xs) livroAlvo = (livro == livroAlvo) || (emprestado2 xs livroAlvo)

--Returns how many books some person has currently rented
qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos xs pessoaAlvo = length [x | x <- xs, fst x == pessoaAlvo]

qtdEmprestimos2 :: BancoDados -> Pessoa -> Int
qtdEmprestimos2 [] pessoaAlvo = 0
qtdEmprestimos2 ((pessoa, livro):xs) pessoaAlvo
	| pessoa == pessoaAlvo = 1 + qtdEmprestimos2 xs pessoaAlvo
	| otherwise =  qtdEmprestimos2 xs pessoaAlvo

--Adds a record of (Pessoa, Livro)
emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar xs pessoa livro = (pessoa, livro):xs

--Removes a record of (Pessoa, Livro)
devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver xs pessoa livro = [x | x <- xs, fst x /= pessoa && snd x /= livro]

devolver2 :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver2 [] _ _ = []
devolver2 ((p,l):xs) pessoa livro
	| pessoa == p && livro == l = (devolver2 xs pessoa livro)
	| otherwise = ((p, l):devolver2 xs pessoa livro)
--Inputs for testing functions
testList1 = [1,2,3,4,5,0]
testList2 = [(1,2),(3,4),(5,0)]
testStr = "leo1nel333a2"

main = do
	putStrLn (show (belongsTo (double testList1) 0) )
	putStrLn (show (belongsTo2 (double testList1) 0) )
	putStrLn (digits testStr)
	putStrLn (show (sumPairs testList2))
	putStrLn (show (livros baseExemplo "Fernando"))
	putStrLn (show (livros2 baseExemplo "Fernando"))
	putStrLn (show (emprestimos baseExemplo "Programming in Haskell"))
	putStrLn (show (emprestimos2 baseExemplo "Programming in Haskell"))
	putStrLn (show (emprestado baseExemplo "Programming in Java"))
	putStrLn (show (emprestado2 baseExemplo "Programming in Haskell"))
	putStrLn (show (qtdEmprestimos baseExemplo "Fernando"))
	putStrLn (show (qtdEmprestimos2 baseExemplo "Fernando"))
	putStrLn (show (emprestar baseExemplo "Lionel" "Programming in Go"))
	putStrLn (show (devolver baseExemplo "Lionel" "Programming in Go"))
	putStrLn (show (devolver2 baseExemplo "Joao" "Software Abstractions"))