-- primeiro programa em haskell
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use max" #-}
{-# HLINT ignore "Use odd" #-}
{-# HLINT ignore "Use even" #-}
{-# HLINT ignore "Use isAsciiLower" #-}
{-# HLINT ignore "Use isAsciiUpper" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

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
    | mod n 2 == 0 = True
    | otherwise = False

-- isPar n = (mod n 2 == 0)

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

----------------------------------------------------------------------------
-- Aula 6 Tuplas

type NomeAluno = String
type NotaAluno = Int
type Aluno = (NomeAluno, NotaAluno)
type Turma = [Aluno]

aprovados :: Turma -> Int -> [NomeAluno]
aprovados tma nota = [ nome | (nome, media) <- tma, media >= nota]

-- Crie a representacao de um ponto de tres dimensoes (x,y,z). 
-- A representacao deve ser realizada por meio de uma tupla e a definicao de um novo tipo.
type Ponto = (Float, Float, Float)
type PontoPar = (Float, Float)

-- Escreva uma funcao que calcule a distancia entre dois pontos passados como argumentos.
distancia :: Ponto -> Ponto -> Float
distancia (x1, y1, z1) (x2, y2, z2) = sqrt ((x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2)

distancia1 :: Ponto -> Ponto -> Float
distancia1 (x1, y1, z1) (x2, y2, z2) = sqrt (d1 + d2 + d3)
    where
        d1 = (x2-x1)^2
        d2 = (y2-y1)^2
        d3 = (z2-z1)^2

pontomedio :: PontoPar -> PontoPar -> PontoPar
pontomedio (x1, y1) (x2, y2) = ((x1+x2)/2, (y1+y2)/2)

quadrante :: PontoPar -> Int
quadrante (x,y) 
    | x > 0 && y > 0 = 1
    | x < 0 && y > 0 = 2
    | x < 0 && y < 0 = 3
    | x > 0 && y < 0 = 4
    | otherwise = 0 

----------------------------------------------------------------------------
-- Aula 7 Casamento de Padroes

padroes :: Int -> String
padroes 1 = "UM"
padroes 2 = "DOIS"
padroes x = "OUTRO"
padroes _ = "OUTRO"

padroes2 :: [Int] -> Int
padroes2 [] = 0
padroes2 (_:xs) = 1 + padroes2 xs

type Tupla4 = (Int,Int,Int,Int)
padroes3 :: Tupla4 -> String
padroes3 (_,_,_,fourth)
    | fourth > 10   = "Maior q 10"
    | otherwise     = "Nao maior q 10"

opp :: (Int, (Int,Int)) -> Int
opp z = if fst z == 1
        then fst (snd z) + snd (snd z)
        else if fst z == 2
            then fst (snd z) - snd (snd z)
            else 0

opp1 :: (Int, (Int,Int)) -> Int
opp1 (1,(x,y)) = x + y
opp1 (2,(x,y)) = x - y
opp1 _ = 0

terceiroelem :: [a] -> a
terceiroelem (_:_:x:_) = x

----------------------------------------------------------------------------
-- Aula 8 Funcoes de Alta Ordem

dobra :: Int -> Int 
dobra n = n + n

dobralista :: [Int] -> [Int]
dobralista [] = []
dobralista (x:xs) = (dobra x):(dobralista xs)

mapInt :: (Int -> Int) -> [Int] -> [Int]
mapInt _ [] = []
mapInt f (x:xs) = (f x):(mapInt f xs)

filtro :: (Int -> Bool) -> [Int] -> [Int]
filtro f [] = []
filtro f (x:xs)
    | (f x) == True = x : (filtro f xs)
    | otherwise     = filtro f xs

isImpar :: Int -> Bool
isImpar x = (mod x 2 == 1)

-- filtro (\x -> mod x 2 == 1) [1,2,3]

maior :: Int -> Int -> Bool
maior a b 
    | a > b = True
    | otherwise = False

menor :: Int -> Int -> Bool
menor a b
    | a < b = True
    | otherwise = False

buscalista :: (Int -> Int -> Bool) -> [Int] -> Int
buscalista _ [] = -1
buscalista _ (x:[]) = x
buscalista f (x:xs) 
    | (f x c) = x
    | otherwise = c
    where c = buscalista f xs

qsort1 :: [Int] -> [Int]
qsort1 [] = []
qsort1 (x:xs) = qsort1 (filter (<= x) xs)
                ++ [x]
                ++ qsort1 (filter (> x) xs)
