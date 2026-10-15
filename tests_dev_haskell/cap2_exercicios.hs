-- by Octávio Teodoro; otacs-dev

-- [1,11,121,1331,14641,161051,1771561]	
listaA = [11^x | x <- [0..6]]

--  [1,2,3,5,6,7,9,10,11,13,14,15,17,18,19,21,22,23,25,26,27,29,30,31,33,34,35,37,38,39]	
listaB = [ x | x <- [1..39], mod x 4 /= 0]

-- 	["AaBB", "AbBB", "AcBB", "AdBB", "AeBB", "AfBB", "AgBB"]	
listaC = [['A' , x , 'B' , 'B'] | x <- ['a'..'g']] 

-- [5,8,11,17,20,26,29,32,38,41]	3 3 6 3 6 3 3 6 3
listaD = takeWhile (<= 41) (scanl (+) 5 (cycle [3, 3, 6, 3, 6, 3, 3, 6, 3]))

-- [1.0,0.5,0.25,0.125,0.0625,0.03125]	
listaE = [1 / (2^x) | x <- [0..5]]

-- [1,10,19,28,37,46,55,64]	
-- listaF = takeWhile (<= 64) (scanl (+) 1 (cycle[9]))
listaF = [1, 10..64]


-- [2,4,8,10,12,16,18,22,24,28,30]	
listaG = takeWhile (<= 30) (scanl (+) 2 (cycle[2,4,2,2,4,2,4,2,4,2]))

-- 1,2,1,1,2,3,2
-- ['@','A','C','D','E','G','J','L']	
listaH :: [Char]
listaH = takeWhile (<= 'L') (scanl (\c n -> toEnum (fromEnum c + n)) '@' (cycle [1,2,1,1,2,3,2]))
-- fromEnum: Letra --> Número
-- toEnum: Número --> Letra

-- Funções de Haskell

--  Crie	 uma	 função	 que	 verifique	 se	 o	 tamanho	 de	 uma
-- String	é	par	ou	não.	Use		Bool		como	retorno.
tamanhoPar :: String -> Bool
tamanhoPar x = even (length x)

reverseString :: String -> String
reverseString [] = []
reverseString (x:y) = reverseString y ++ [x]

tamanhosImpares :: [String] -> [Int]
tamanhosImpares [] = []
tamanhosImpares (x:y) = if odd (length x) 
    then length x : tamanhosImpares y
     else tamanhosImpares y

primeiroElementoComposicao :: [a] -> a
primeiroElementoComposicao = last . reverse

ehPalindromo :: String -> Bool
ehPalindromo texto = texto == reverse texto


gerarMultiplosEmTupla :: Int -> (Int, Int, Int, Int)
gerarMultiplosEmTupla numero = (numero * 2, numero * 3, numero * 4, numero * 5)












