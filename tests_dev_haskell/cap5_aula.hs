-- fib :: Int -> Int
-- fib 0 = 0
-- fib 1 = 1
-- fib 2 = 2
-- fib n 
   -- | n < 0 = 0
   -- | otherwise = fib (n-1) +  fib(n-2)


-- fat' :: (Int, Int) -> Int
-- fat' (0,resultado) = resultado
-- fat' (indice,resultado) = fat' (indice-1, resultado * indice)

-- fat'' x = fat'(x,1)
-- currying

-- tipos parametricos - polimorfismo parametrico


data Caixa  a = UmaCaixa a | DuasCaixas a a deriving Show
-- tipos kind 2
data CaixaPolimorficas b c = UmaCaixaPolimorficas b | DuasCaixasPolimorficas b c deriving Show
-- tipos kind 3


data Arvore a = Galho a (Arvore a) (Arvore a ) | Folha a 
    | Nulo deriving (Show,Eq)

arvereNozes = Galho 40 (Galho 25 (Folha 20)(Folha 30)) Nulo

InserirElemento :: (Ord a) => Arvore a -> a -> Arvore a
InserirElemento Nulo eN = Folha eN
InserirElemento (Folha x) eN 
    | eN < x = Galho x (Folha eN) Nulo
    | eN >= x = Galho x Nulo (Folha eN) 
InserirElemento (Galho x esq dir) eN
    | eN <= x = Galho x (InserirElemento esq eN) dir
    | eN > x = Galho x esq (InserirElemento dir eN)





  




























