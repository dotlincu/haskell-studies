-- https://blog.haskellbr.com/2015/12/04/implementando-fibonacci-em-haskell.html
-- https://www.facom.ufu.br/~madriana/PF/tutorial_avancado.pdf
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use odd" #-}

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