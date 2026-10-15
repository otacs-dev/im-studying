-- Funções Lambdas

-- (\x -> x + 1)10
-- (\x y -> x + y) 10 20
-- (\x y z -> x + y - z) 10 20 5

-- :t map    ->    map :: (a -> b) -> [a] -> [b]

-- (\ x y z -> x ++ y ++ z) "Palmeiras " " Nao" " Tem Mundial"      ->       "Palmeiras Nao Tem Mundial"

-- map (\x -> x / 1) [1,2,3]      ->       [1.0,2.0,3.0]

-- zip [1,2,3] [3,4,5]         ->          [(1,3),(2,4),(3,5)]

-- zipWith (\ a b -> a * b) [1,2,3] [4,5,6]    ->     [4,10,18]

-- sum $ zipWith (\ a b -> a * b) [1,2,3] [4,5,6]    ->    32

-- map (\ x -> elem x "AEIOUaeiou") "Palmeiras"        ->         [False,True,False,False,True,True,False,True,False]

-- filter (\ x -> elem x "AEIOUaeiou") "Palmeiras"      ->       "aeia"

-- filter even [1,2,3,4,5]       ->        [2,4]

-- foldl (+) 0 [1,2,3]         ->         6

-- mapa :: (a -> b) -> [a] -> [b]
-- mapa funcao xs = [funcao x | x <- xs]

-- type Idade = Int

-- faixaEtaria :: Idade -> String
-- faixaEtaria idade
--  | idade <= 12 = "Crianca"
--  | idade <= 17 = "Adolescente"
--  | otherwise = "Adulto"

-- fib :: Int -> Int
-- fib 0 = 0
-- fib 1 = 1
-- fib 2 = 1
-- fib n = fib (n-1) + fib(n-2)
-- implementar foldl e matriz


















