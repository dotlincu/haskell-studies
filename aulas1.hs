-- primeiro programa em haskell
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use max" #-}
{-# HLINT ignore "Use odd" #-}
{-# HLINT ignore "Use even" #-}
{-# HLINT ignore "Use isAsciiLower" #-}
{-# HLINT ignore "Use isAsciiUpper" #-}

-- Aula 1 Introducao
polinomio :: Int -> Int
polinomio x = x * x + 10*x + 2

main = do
    putStrLn "Hello World!"

soma :: Int-> Int -> Int
soma a b = a + b

hip :: Float -> Float -> Float
hip a b = sqrt (a*a + b*b)

area :: Float -> Float
area a = a * a * pi

dif :: Float -> Float -> Float
dif a b = abs (area a - area b)
----------------------------------------------------------------------------
-- Aula 2 Condicionais
maior1 :: Int -> Int -> Int
maior1 a b = if a > b
    then a
    else b

maior2 :: Int -> Int -> Int -> Int -> Int
maior2 a b c d = if a + b > c + d
    then a + b
    else c + d

fatorial :: Int -> Int
fatorial n
    | n == 0 = 1
    | otherwise = n * fatorial (n-1)

maiorg :: Int -> Int -> Int
maiorg a b
    | a > b = a
    | a < b = b
    | otherwise = 0

isPar :: Int -> Bool
isPar n
    | n `mod` 2 == 0 = True
    | otherwise = False

charcase :: Char -> String
charcase c
    | c >= 'a' && c <= 'z' = "minusculo"
    | c >= 'A' && c <= 'Z' = "maiusculo"
    | otherwise = "nao eh letra"

funcao :: Int -> Int -> Int -> Int
funcao a b c
    | a == 0 = b^2 + 3*c
    | a == 1 = 2*c^2 - 3*c
    | a == 2 = 3*c - b^2
    | otherwise = 0

----------------------------------------------------------------------------
-- Aula 3 Definicoes Locais
areaheron :: Float -> Float -> Float -> Float
areaheron a b c = sqrt (s*(s-a)*(s-b)*(s-c))
    where s = (a+b+c)/2

funcao1 :: Int -> Int -> Int
funcao1 x y
    | x <= 10 = x + a
    | otherwise = x - a
    where
        a = 2*y

funcao2 :: Int -> Int
funcao2 y = 3 + func y + func a + func b
    where
        func x = x + 7*c
        a = 3*c
        b = func 2
        c = 10

eq2grau :: Float -> Float -> Float -> Int
eq2grau a b c
    | delta > 0 = 2
    | delta == 0 = 1
    | otherwise = 0
    where
        delta = b^2 - 4*a*c

-- let x = 4+6 in x*x
-- let x = 4+6; y = 2*x in x*y

areacilindro :: Float -> Float -> Float
areacilindro r h = let arealado = 2*pi*r*h
                       areabase = pi*r^2
                   in 2*areabase + arealado

areaheron2 :: Float -> Float -> Float -> Float
areaheron2 a b c = let s = (a+b+c)/2
    in sqrt (s*(s-a)*(s-b)*(s-c))

----------------------------------------------------------------------------
-- Aula 4 Recursao
divrec :: Int -> Int -> Int
divrec a b
    | a < b     = a
    | a == b    = 0
    | otherwise = divrec (a-b) b

multi :: Int -> Int -> Int
multi x n
    | n == 0 = 0
    | n == 1 = x
    | otherwise = x + multi x (n-1)

mdc :: Int -> Int -> Int
mdc x y
    | x > y = mdc (x-y) y
    | x < y = mdc y x
    | otherwise = x

potencia2 :: Int -> Int
potencia2 n
    | n == 0 = 1
    | n > 0  = 2 * potencia2 (n-1)

potencia2cauda :: Int -> Int -> Int
potencia2cauda n acumulado
    | n == 0 = acumulado
    | n > 0  = potencia2cauda (n-1) (2*acumulado)

fatrec :: Int -> Int -> Int
fatrec n acc
    | n == 0 = acc
    | n > 0  = fatrec (n-1) (n*acc)

fibocauda :: Int -> Int -> Int -> Int
fibocauda n acc1 acc2
    | n == 0 = acc1
    | n == 1 = acc2
    | n > 1  = fibocauda (n-1) acc2 (acc1+acc2)

----------------------------------------------------------------------------
-- Aula 5 Listas

lista = 1 : [1,2,3,4,5,6,7,8,9,10]

comp :: [Int] -> Int
comp [] = 0
comp (x:xs) = 1 + comp xs

comp1 :: [Int] -> Int
comp1 lista
    | null lista = 0
    | otherwise = 1 + comp1 (tail lista)

cubo :: Int -> Int
cubo x = x * x * x
aoCubo :: [Int] -> [Int]
aoCubo [] = []
aoCubo (x:xs) = cubo x : aoCubo xs

somatoria :: [Int] -> Int
somatoria [] = 0
somatoria (x:xs) = x + somatoria xs

verifica :: Char -> [Char] -> Bool
verifica c [] = False
verifica c (x:xs) 
    | x == c = True 
    | otherwise = verifica c xs

maiorv :: [Int] -> Int
maiorv [] = -1
maiorv (x:xs)
    | x >= maiorx = x
    | otherwise = maiorx
    where maiorx = maiorv xs

raizes :: Float -> Float -> Float -> [Float]
raizes a b c 
    | delta < 0     = []
    | delta == 0    = [(-b)/(2*a)]
    | otherwise     = [(-b - sqrt delta)/(2*a), (-b + sqrt delta)/(2*a)]
    where delta = b*b - 4*a*c 

func3 :: Int -> [Int]
func3 n = [x | x <- [1..n], mod n x == 0]

multiplos :: Int -> [Int]
multiplos n = [ n * x | x <- [1..10]]

isPrimo :: Int -> Bool
isPrimo n
    | length [x | x <- [1..n], mod n x == 0] == 2 = True
    | otherwise = False

primos :: Int -> [Int]
primos n = [x | x <- [1..n], isPrimo x]

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort [y | y <- xs, y <= x] 
    ++ [x] 
    ++ qsort [y | y <- xs, y > x]

