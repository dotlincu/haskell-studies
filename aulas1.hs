-- primeiro programa em haskell
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use max" #-}
{-# HLINT ignore "Use odd" #-}
{-# HLINT ignore "Use even" #-}
{-# HLINT ignore "Use isAsciiLower" #-}
{-# HLINT ignore "Use isAsciiUpper" #-}

-- Aula 1
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
-- Aula 2
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
-- Aula 3
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
