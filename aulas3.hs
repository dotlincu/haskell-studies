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