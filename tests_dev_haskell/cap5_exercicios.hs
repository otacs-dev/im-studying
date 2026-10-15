-- (Monoides, Typeclasses, Árvores)


-- 5.1) Produto e Monoide

data TipoProduto = Escritorio | Informatica | Livro | Filme | Total
  deriving (Show, Eq)

data Produto = Produto { valor :: Double, tp :: TipoProduto }
             | Nada
  deriving Show

valorDe :: Produto -> Double
valorDe Nada          = 0
valorDe (Produto v _) = v

-- somar dois produtos: soma os valores e o tipo vira Total
instance Semigroup Produto where
  a <> b = Produto (valorDe a + valorDe b) Total

instance Monoid Produto where
  mempty = Nada

-- Sem monoides:
--   Teríamos que escrever uma função de soma na mão, com recursão:

--     somaLista :: [Produto] -> Double
--     somaLista []       = 0
--     somaLista (p : ps) = valorDe p + somaLista ps

--   Diferenças:
--   - Teríamos que tratar o caso "lista vazia" e o "Nada" manualmente.
--   - A função serviria só para Produto, não dá para reaproveitar.
--   - Com monoide, basta usar <> e mconcat, e o mesmo jeito
--    de combinar vale para qualquer tipo que seja monoide.



-- 5.2) totalGeral
-- junta todos os produtos com mconcat e pega o valor
totalGeral :: [Produto] -> Double
totalGeral lista = valorDe (mconcat lista)





-- 5.3) Min
data Min = Min Int
  deriving (Eq, Ord, Show)

instance Semigroup Min where
  Min a <> Min b = Min (min a b)

instance Monoid Min where
  mempty = Min maxBound




-- 5.4) minAll
minAll :: [Min] -> Min
minAll lista = mconcat lista




-- 5.5) Paridade e typeclass ParImpar
data Paridade = Par | Impar
  deriving (Show, Eq)

class ParImpar a where
  decide :: a -> Paridade

-- Int: par se o resto da divisão por 2 for 0
instance ParImpar Int where
  decide n
    | even n    = Par
    | otherwise = Impar

-- Lista: é par se o tamanho dela for par
instance ParImpar [a] where
  decide lista
    | even (length lista) = Par
    | otherwise           = Impar

-- Bool: False é Par, True é Impar
instance ParImpar Bool where
  decide False = Par
  decide True  = Impar




-- 5.6) Max
data Max = Max Int
  deriving (Eq, Ord, Show)

instance Semigroup Max where
  Max a <> Max b = Max (max a b)
-- mempty = minBound, porque qualquer número é maior ou igual a ele.

instance Monoid Max where
  mempty = Max minBound

maxAll :: [Max] -> Max
maxAll lista = mconcat lista



--                   |
--                   |
--                   ↓ 

-- --------------------------------------------------------------
                                                        --  |
-- Estrutura de árvore (usada no próximo em diante)     --  |
data Arvore a = Nula                                    --  |
              | Folha a                                 --  |
              | Raiz a (Arvore a) (Arvore a)            --  | 
  deriving Show                                         --  |
                                                        --  |  
-- --------------------------------------------------------------




-- 5.7) mapa: aplica uma função em todos os elementos da árvore
mapa :: (a -> b) -> Arvore a -> Arvore b
mapa _ Nula                = Nula
mapa f (Folha x)           = Folha (f x)
mapa f (Raiz x esq dir)    = Raiz (f x) (mapa f esq) (mapa f dir)






-- 5.8) Somar 5 em cada elemento
somaCinco :: Arvore Int -> Arvore Int
somaCinco arvore = mapa (+ 5) arvore






-- Exercício 5.9 - Lista ordenada
infixr 5 :?:
data ListaOrd a = a :?: (ListaOrd a)
                | Nulo
  deriving Show

-- insere mantendo a ordem crescente
inserir :: (Ord a) => a -> ListaOrd a -> ListaOrd a
inserir x Nulo = x :?: Nulo
inserir x (y :?: resto)
  | x <= y    = x :?: (y :?: resto)   -- achou o lugar
  | otherwise = y :?: inserir x resto -- continua procurando

-- remove o elemento; se não achar, a lista fica igual
remover :: (Eq a) => a -> ListaOrd a -> ListaOrd a
remover _ Nulo = Nulo
remover x (y :?: resto)
  | x == y    = resto
  | otherwise = y :?: remover x resto

-- conta quantos elementos tem
tamanho :: ListaOrd a -> Int
tamanho Nulo          = 0
tamanho (_ :?: resto) = 1 + tamanho resto

-- DESCOMENTAR PARA TESTE INDIVIDUAL
-- main :: IO ()
-- main = do
--  print (inserir 3 (inserir 1 (inserir 2 Nulo)))
--  print (remover 2 (inserir 3 (inserir 1 (inserir 2 Nulo))))
--  print (remover 9 (inserir 3 (inserir 1 (inserir 2 Nulo))))
--  print (tamanho (inserir 3 (inserir 1 (inserir 2 Nulo))))





-- 5.10) Somar todos os elementos da árvore
somaArvore :: (Num a) => Arvore a -> a
somaArvore Nula             = 0
somaArvore (Folha x)        = x
somaArvore (Raiz x esq dir) = x + somaArvore esq + somaArvore dir







-- 5.11) Percursos pré-ordem e pós-ordem
-- pré-ordem: raiz, esquerda, direita
preOrdem :: Arvore a -> [a]
preOrdem Nula             = []
preOrdem (Folha x)        = [x]
preOrdem (Raiz x esq dir) = [x] ++ preOrdem esq ++ preOrdem dir

-- pós-ordem: esquerda, direita, raiz
posOrdem :: Arvore a -> [a]
posOrdem Nula             = []
posOrdem (Folha x)        = [x]
posOrdem (Raiz x esq dir) = posOrdem esq ++ posOrdem dir ++ [x]

arvoreExemplo :: Arvore Int
arvoreExemplo =
  Raiz 15
    (Raiz 11 (Folha 6) (Raiz 12 (Folha 10) Nula))
    (Raiz 20 Nula (Raiz 22 (Folha 21) Nula))







-- 5.12) Inserir em árvore de busca binária
-- menores vão para a esquerda, maiores para a direita,
-- repetidos são ignorados

inserirArv :: (Ord a) => a -> Arvore a -> Arvore a
inserirArv x Nula = Folha x
inserirArv x (Folha y)
  | x < y     = Raiz y (Folha x) Nula
  | x > y     = Raiz y Nula (Folha x)
  | otherwise = Folha y
inserirArv x (Raiz y esq dir)
  | x < y     = Raiz y (inserirArv x esq) dir
  | x > y     = Raiz y esq (inserirArv x dir)
  | otherwise = Raiz y esq dir





-- 5.13) Inserir uma lista inteira na árvore
-- vai inserindo um elemento de cada vez, da esquerda para a direita
inserirLista :: (Ord a) => [a] -> Arvore a -> Arvore a
inserirLista lista arvore = foldl (flip inserirArv) arvore lista


-- Testes rápidos (rode com: main)
main :: IO ()
main = do
  print (totalGeral [Produto 10 Livro, Produto 25.5 Filme, Nada])
  print (Min (-32) <> Min (-34) <> Min (-33))
  print (minAll [Min 4, Min 2, Min 9])
  print (Max 10 <> Max 13 <> Max 5)
  print (maxAll [Max 4, Max 2, Max 9])
  print (decide (4 :: Int), decide [1, 2, 3 :: Int], decide True)
  print (somaCinco arvoreExemplo)
  print (somaArvore arvoreExemplo)
  print (preOrdem arvoreExemplo)
  print (posOrdem arvoreExemplo)
  print (inserir 3 (inserir 1 (inserir 2 Nulo)))
  print (remover 2 (inserir 3 (inserir 1 (inserir 2 Nulo))))
  print (tamanho (inserir 3 (inserir 1 (inserir 2 Nulo))))
  print (inserirLista [15, 11, 20, 6] Nula)