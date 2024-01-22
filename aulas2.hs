-- https://blog.haskellbr.com/2015/12/04/implementando-fibonacci-em-haskell.html
-- https://www.facom.ufu.br/~madriana/PF/tutorial_avancado.pdf
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use odd" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use product" #-}

-- raiz_quadrada x = map sqrt x

soma10 :: Num a => a -> a
soma10 x = x + 10
quadrado :: Num a => a -> a
quadrado x = x * x

isImpar :: Int -> Bool
isImpar n = n `mod` 2 /= 0

duplica :: [Int] -> [Int]
duplica [] = []
duplica (x:xs) = x : x : duplica xs

-- result :: [Int] -> [Int]
-- result = map (\x -> x * x) (filter isImpar (x:xs))

somaQI :: [Int] -> Int
somaQI [] = 0
somaQI xs = foldr (\item acc -> if odd item then item * item + acc else acc) 0 xs

produtoLI :: [Int] -> Int
produtoLI xs = foldr1 (*) xs

produtoLI2 :: [Int] -> Int
produtoLI2 xs = foldr (*) 1 xs
