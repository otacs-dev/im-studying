-- ---------------------------------------------------------


--1)-- A) 
lM :: String -> Int 
lM texto = length $ filter( \x -> elem x ['A'..'Z'] ) texto 


--1)-- B)


somaPares :: [Int] -> Int
somaPares xs= foldl (+) 0 $ filter even xs





--1)--\ C)

mVinte :: [Int] -> [Int]
mVinte xs = filter (>=20) xs



--1)-- D)

triplica :: [Double] -> [Double]
triplica xs = map (*3) xs


-- -------------------------------


--2)--A) --map com reverse

-- bar :: (a -> b) -> [a] -> [b]
-- bar [] = []
-- bar f (x:xs) = bar’ f xs ++ [(f x)]

-- bar ( \ x -> elem x "FEfe") "ATEC" ++ [(( \ x -> elem x "FEfe")'F')]


-- ( Teste de Mesa )
-- bar f ('F':"ATEC") = bar f "ATEC" ++ [f 'F']
-- bar f ('A' : "TEC") = bar f "TEC" ++ [(f 'A')]
-- bar f ('T':"EC") = bar f "EC" ++ [(f 'T')]
-- bar f ('E': "C") = bar f "C" ++ [(f 'E')]
-- bar f ('C':"") = bar f "" ++ [(f 'C')]
-- bar _ [] = []


-- ---------------------------------------------------------


--3)--A) 

data Pessoa = Fisica {nome::String, cpf::String}
            | Juridica {nome::String, cnpj::String} deriving Show
        
--3)--B)

ehFisica :: Pessoa -> Bool
ehFisica Fisica{} = True
ehFisica _ = False

pessoasFisica :: [Pessoa] -> [Pessoa]
pessoasFisica = filter ehFisica

--3)--C)

totalPessoasFisicas :: [Pessoa] -> Int
totalPessoasFisicas xs = length $ pessoasFisica xs


-- ---------------------------------------------------------

--4)

maior :: [Int] -> Int
maior [] = error "FP"
maior [x] = x
maior (x:xs) 
    | x > m = x
    | otherwise = m
    where m = maior xs


-- ---------------------------------------------------------

--5)--A)
-- map (\x -> x - x + x) [8,2,3] = [8,2,3]


--5)--B)
-- :t filter (\x -> x /= 93) [90..100] :: (Eq a, Num a, Enum a) => [a]


--5)--C)
-- :t ("3",4) :: (String, Int)



--5)--D)
-- foldl (\ a b -> a - b) 0 [5..10] == -45



                       














