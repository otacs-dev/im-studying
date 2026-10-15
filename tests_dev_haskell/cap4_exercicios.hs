-- 4.1) Média de um [Double] usando foldl
media :: [Double] -> Double
media [] = 0
media xs = soma / tamanho
  where 
    soma = foldl (+) 0 xs
    tamanho = fromIntegral (length xs)

-- 4.2) Filtrar os elementos palíndromos de uma [String]
palindromos :: [String] -> [String]
palindromos = filter (\s -> s == reverse s)

-- 4.3) Filtrar os números pares e ímpares
filtraPares :: [Int] -> [Int]
filtraPares = filter even

filtraImpares :: [Int] -> [Int]
filtraImpares = filter odd

-- 4.4) Filtrar os números primos
-- Função auxiliar para verificar se é primo
ehPrimo :: Int -> Bool
ehPrimo n
  | n <= 1    = False
  | otherwise = length [x | x <- [1..n], n `mod` x == 0] == 2

filtraPrimos :: [Int] -> [Int]
filtraPrimos = filter ehPrimo

-- 4.5) Dobro de todos, eliminando os múltiplos de 4
dobroSemMult4 :: [Int] -> [Int]
dobroSemMult4 xs = filter (\x -> x `mod` 4 /= 0) (map (*2) xs)

-- 4.6) Função que recebe f e string s, retorna reverso(s) ++ f(s)
func :: (String -> String) -> String -> String
func f s = reverse s ++ f s

-- 4.7) Tipo Dia e função para filtrar Terças
data Dia = Domingo | Segunda | Terca | Quarta | Quinta | Sexta | Sabado 
  deriving (Show, Eq)

filtraTercas :: [Dia] -> [Dia]
filtraTercas = filter (== Terca)

-- 4.8) Tipo Dinheiro
data Correncia = Real | Dolar deriving (Show, Eq)

data Dinheiro = Dinheiro {
    valor :: Double,
    correncia :: Correncia
} deriving (Show)

-- Assumindo uma taxa de conversão fictícia (ex: 1 Dolar = 5 Reais)
taxaConversao :: Double
taxaConversao = 5.0

paraDolar :: Dinheiro -> Dinheiro
paraDolar (Dinheiro v Dolar) = Dinheiro v Dolar
paraDolar (Dinheiro v Real)  = Dinheiro (v / taxaConversao) Dolar

paraReal :: Dinheiro -> Dinheiro
paraReal (Dinheiro v Real)  = Dinheiro v Real
paraReal (Dinheiro v Dolar) = Dinheiro (v * taxaConversao) Real

-- Filtrar todos os dólares de uma lista
filtraDolares :: [Dinheiro] -> [Dinheiro]
filtraDolares = filter (\(Dinheiro _ c) -> c == Dolar)

-- Somar todos os dólares de uma lista
somaDolares :: [Dinheiro] -> Double
somaDolares xs = sum (map valor (filtraDolares xs))

-- Contar a quantidade de dólares
contaDolares :: [Dinheiro] -> Int
contaDolares xs = length (filtraDolares xs)


-- 4.9) Usando foldl para criar lambdas
-- Contar números negativos
contaNegativos :: [Int] -> Int
contaNegativos = foldl (\acc x -> if x < 0 then acc + 1 else acc) 0

-- Contar letras 'P'
contaP :: String -> Int
contaP = foldl (\acc c -> if c == 'P' then acc + 1 else acc) 0

-- Contar Sábados
contaSabados :: [Dia] -> Int
contaSabados = foldl (\acc d -> if d == Sabado then acc + 1 else acc) 0

-- Somar dias (Segunda = 1, Terca = 2, Quarta = 3 para [Segunda, Segunda, Quarta] = 5)
diaParaInt :: Dia -> Int
diaParaInt Domingo = 0 
diaParaInt Segunda = 1
diaParaInt Terca   = 2
diaParaInt Quarta  = 3
diaParaInt Quinta  = 4
diaParaInt Sexta   = 5
diaParaInt Sabado  = 6

somaDias :: [Dia] -> Int
somaDias = foldl (\acc d -> acc + diaParaInt d) 0


-- 4.10) Reescrevendo funções com . , $ e |>
-- Obs: O operador |> (forward pipe) não é nativo do Prelude do Haskell. 
-- Em alguns materiais didáticos ele é definido assim:
(|>) :: a -> (a -> b) -> b
x |> f = f x
infixl 0 |>

-- Refazendo o exercício 4.5 com composição (.)
dobroSemMult4Comp :: [Int] -> [Int]
dobroSemMult4Comp = filter (\x -> x `mod` 4 /= 0) . map (*2)

-- Refazendo o exercício 4.5 com pipe (|>)
dobroSemMult4Pipe :: [Int] -> [Int]
dobroSemMult4Pipe xs = xs |> map (*2) |> filter (\x -> x `mod` 4 /= 0)

-- Refazendo somaDolares do 4.8 com ($) e (.)
somaDolaresComp :: [Dinheiro] -> Double
somaDolaresComp xs = sum $ map valor$ filtraDolares xs

somaDolaresPonto :: [Dinheiro] -> Double
somaDolaresPonto = sum . map valor . filtraDolares