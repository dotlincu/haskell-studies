reverse1 :: [Int] -> [Int]
reverse1 [] = []
reverse1 (x:xs) = reverse1 xs ++ [x]

isPrimo :: Int -> Bool
isPrimo n 
    | length [x | x <- [1..n], mod n x == 0 ] == 2 = True
    | otherwise = False

somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x:xs) = x + somaLista xs

insere :: Int -> [Int] -> [Int]
insere e [] = [e]
insere e (x:xs)
    | e <= x    = e:(x:xs)
    | otherwise = x:insere e xs
ordenacao :: [Int] -> [Int]
ordenacao [] = []
ordenacao (x:xs) = insere x (ordenacao xs)

somaPares :: [(Int,Int)] -> [Int]
somaPares (x:xs) = [a+b|(a,b)<-(x:xs)]

pares :: [t] -> [u] -> [(t,u)]
pares lista1 lista2 = [(a,b)|a <- lista1, b <- lista2]

remove :: Char -> [Char] -> [Char]
remove c str = [a | a <- str, a/= c]

ignore :: Int -> [Int] -> [Int]
ignore _ [] = []
ignore n (x:xs) 
    | n == 0 = x : ignore n xs
    | otherwise = ignore (n-1) xs


--- Lista VI

temp :: Float -> Float
temp c = 9/5 * c + 32

isPar :: Int -> Bool
isPar n = (mod n 2 == 0)

isImpar :: Int -> Bool
isImpar n = (mod n 2 == 1)

maior :: [Int] -> Int
maior [] = -1
maior (x:xs)
    | x >= maior xs = x
    | otherwise = maior xs

func :: Int -> [Int] -> [Int]
func _ [] = []
func n lista = filter (>n) lista

func1 :: [Int] -> [Int]
func1 [] = []
func1 (x:xs) = x:x:func1 xs

func2 :: Int -> Bool
func2 n = length [x | x <- [1..n], mod n x == 0] == 2

numerosPar :: [Int] -> [Int]
numerosPar [] = []
numerosPar (x:xs) = filter (isPar) (x:xs)

func3 :: [Int] -> Int
func3 [] = 0
func3 (x:xs) 
    | isImpar x = x^2 + func3 xs
    | otherwise = func3 xs

func4 :: [Int] -> Int
func4 [] = 0
func4 (x:xs) = foldr (\n acc -> if odd n then n*n + acc else acc) 0 (x:xs)

func5 :: [Int] -> Int
func5 [] = 0
func5 (x:xs) = foldr1 (*) (x:xs)


func6 :: [String] -> [Int]
func6 [] = []
func6 lista = map (length) (lista)