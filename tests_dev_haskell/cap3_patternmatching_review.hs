data Booleano = Verdade | Falso | Erro deriving Show

boolParaInt :: Booleano -> Int
boolParaInt  Falso = 0
boolParaInt  Verdade = 1

intParaBool :: Int -> Booleano
intParaBool 0 = Falso
intParaBool 1 = Verdade
intParaBool x = Erro
intParaBool' :: Int -> Booleano
intParaBool' x = if x == 0 then Falso else if x == 1 then Verdade else Erro













