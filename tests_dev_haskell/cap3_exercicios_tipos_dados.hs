-- by Octávio Teodoro; otacs-dev

-- PATTERN MATCHING
-- VALUES CONSTRUCTORS
-- RECORD SYNTAX


-- 3.1) Crie o tipo Pergunta com os values constructors Sim ou Nao.
-- Faça as funções seguintes, determinando seus tipos explicitamente.
-- pergNum: recebe via parâmetro uma Pergunta. Retorna 0 para Nao; e 1 para Sim.
-- listPergs: recebe via parâmetro uma lista de perguntas, e retorna 0s e 1s correspondentes aos constructores contidos na lista.
-- and': recebe duas Perguntas como parâmetro e retorna a tabela verdade do and lógico, usando Sim como verdadeiro e Nao como falso.
-- or': idem ao anterior, porém deve ser usado o "or" lógico.
-- not': idem aos anteriores, porém usando o not lógico.

data Pergunta = Sim | Nao deriving (Show, Eq)

pergNum :: Pergunta -> Int
pergNum Nao = 0
pergNum Sim = 1


listPergs :: [Pergunta] -> [Int]
listPergs ps = [pergNum p | p <- ps]


and' :: Pergunta -> Pergunta -> Pergunta
and' Sim Sim = Sim
and' _   _   = Nao

 
or' :: Pergunta -> Pergunta -> Pergunta
or' Nao Nao = Nao
or' _   _   = Sim

 
not' :: Pergunta -> Pergunta
not' Sim = Nao
not' Nao = Sim
 
------------------------------------------------------------------------------------------------------------------

-- 3.2) Faça o tipo Temperatura que pode ter valores Celsius, Farenheit ou Kelvin.
-- Implemente as funções: 
-- converterCelsius: recebe um valor double e uma temperatura, e faz a conversão para Celsius.
-- converterKelvin: recebe um valor double e uma temperatura, e faz a conversão para Kelvin.
-- converterFarenheit: recebe um valor double e uma temperatura, e faz a conversão para Farenheit.


data Temperatura = Celsius | Farenheit | Kelvin deriving (Show, Eq)


converterCelsius :: Double -> Temperatura -> Double
converterCelsius v Celsius   = v
converterCelsius v Farenheit = (v - 32) * 5 / 9
converterCelsius v Kelvin    = v - 273.15


converterKelvin :: Double -> Temperatura -> Double
converterKelvin v Celsius   = v + 273.15
converterKelvin v Farenheit = (v - 32) * 5 / 9 + 273.15
converterKelvin v Kelvin    = v


converterFarenheit :: Double -> Temperatura -> Double
converterFarenheit v Celsius   = v * 9 / 5 + 32
converterFarenheit v Farenheit = v
converterFarenheit v Kelvin    = (v - 273.15) * 9 / 5 + 32


------------------------------------------------------------------------------------------------------------------


-- 3.3) Implemente uma função que simule o vencedor de uma partida de pedra, papel e tesoura usando tipos criados.
-- Casos de empate devem ser considerados em seu tipo.

data Jogada = Pedra | Papel | Tesoura deriving (Show, Eq)
data Resultado = Jogador1 | Jogador2 | Empate deriving (Show, Eq)

vencedor :: Jogada -> Jogada -> Resultado
vencedor a b
  | a == b                        = Empate
  | a == Pedra   && b == Tesoura  = Jogador1
  | a == Papel   && b == Pedra    = Jogador1
  | a == Tesoura && b == Papel    = Jogador1
  | otherwise                     = Jogador2


------------------------------------------------------------------------------------------------------------------


-- 3.4) Faça uma função que retorne uma string, com todas as vogais maiúsculas e minúsculas eliminadas de uma string 
-- passada por parâmetro usando list compreenshion.

removerVogais :: String -> String
removerVogais s = [c | c <- s, c `notElem` "aeiouAEIOU"]


------------------------------------------------------------------------------------------------------------------


-- 3.5) Sabe-se que as unidades imperiais de comprimento podem ser Inch, Yard ou Foot (há outras ignoradas aqui).
-- Sabe-se que 1in=0.0254m, 1yd=0.9144m, 1ft=0.3048m. Faça a função converterMetros que recebe a unidade imperial
-- e o valor correspondente nesta unidade. Esta função deve retornar o valor em metros.
-- Implemente também a função converterImperial, que recebe um valor em metros e a unidade de conversão.
-- Esta função deve retornar o valor convertido para a unidade desejada.

data UnidadeImperial = Inch | Yard | Foot deriving (Show, Eq)

converterMetros :: UnidadeImperial -> Double -> Double
converterMetros Inch v = v * 0.0254
converterMetros Yard v = v * 0.9144
converterMetros Foot v = v * 0.3048

converterImperial :: Double -> UnidadeImperial -> Double
converterImperial v Inch = v / 0.0254
converterImperial v Yard = v / 0.9144
converterImperial v Foot = v / 0.3048


------------------------------------------------------------------------------------------------------------------


-- 3.6) Faça um novo tipo chamado Mes, que possui como valores todos os meses do ano. 
-- Implemente:
-- A função checaFim, que retorna o número de dias que cada mês possui (considere fevereiro tendo 28 dias).
-- A função prox, que recebe um mês atual e retorna o próximo mês.
-- A função estacao, que retorna a estação do ano de acordo com o mês e com o hemisfério.

data Mes = Janeiro | Fevereiro | Marco | Abril | Maio | Junho
         | Julho | Agosto | Setembro | Outubro | Novembro | Dezembro
         deriving (Show, Eq, Enum, Bounded)


data Hemisferio = Norte | Sul deriving (Show, Eq)
data Estacao = Verao | Outono | Inverno | Primavera deriving (Show, Eq)


checaFim :: Mes -> Int
checaFim Fevereiro = 28
checaFim m
  | m `elem` [Abril, Junho, Setembro, Novembro] = 30
  | otherwise                                   = 31


prox :: Mes -> Mes
prox Dezembro = Janeiro
prox m        = succ m


estacao :: Mes -> Hemisferio -> Estacao
estacao m Sul
  | m `elem` [Dezembro, Janeiro, Fevereiro] = Verao
  | m `elem` [Marco, Abril, Maio]           = Outono
  | m `elem` [Junho, Julho, Agosto]         = Inverno
  | otherwise                               = Primavera
estacao m Norte
  | m `elem` [Dezembro, Janeiro, Fevereiro] = Inverno
  | m `elem` [Marco, Abril, Maio]           = Primavera
  | m `elem` [Junho, Julho, Agosto]         = Verao
  | otherwise                               = Outono


------------------------------------------------------------------------------------------------------------------


-- 3.7) Faça uma função que receba uma String e retorne True se esta for um palíndromo; caso contrário, False.

palindromo :: String -> Bool
palindromo s = s == reverse s


------------------------------------------------------------------------------------------------------------------


-- 3.8) Faça uma função que elimine todos os números pares, todos os ímpares múltiplos de 7 e negativos de uma lista de inteiros 
-- passada via parâmetro. Você deve retornar esta lista em ordem reversa em comparação a do parâmetro.

filtrarLista :: [Int] -> [Int]
filtrarLista xs = reverse [x | x <- xs,
                                not (even x),
                                not (odd x && x `mod` 7 == 0),
                                x >= 0]


------------------------------------------------------------------------------------------------------------------


-- 3.9) Faça uma função que recebe três Strings x, y e z como parâmetro. A função retorna uma tupla com três coordenadas
-- contendo a ordem reversa em cada.
-- A primeira coordenada deve conter string reversa do primeiro parâmetro, e assim por diante.

reverseTuple :: String -> String -> String -> (String, String, String)
reverseTuple x y z = (reverse x, reverse y, reverse z)


------------------------------------------------------------------------------------------------------------------


-- 3.10) Faça uma função chamada revNum, que receba uma String s e um Int n.
-- Esta deverá retornar as n primeiras letras em ordem reversa e o restante em sua ordem normal.

-- Exemplo:
-- revNum 4 "FATEC" = "ETAFC"

revNum :: Int -> String -> String
revNum n s = reverse (take n s) ++ drop n s
-- revNum 4 "FATEC" = "ETAFC"


------------------------------------------------------------------------------------------------------------------


-- 3.11) Crie o tipo de dado Binario que pode ser Zero ou Um. Faça outro tipo de dado chamado Funcao que pode ser
-- Soma2, Maior, Menor ou Mult2.
-- Implemente a função aplicar que recebe uma Funcao e dois Binarios. Seu retorno consiste em executar a operação desejada. 

-- Exemplo:
-- aplicar Soma2 Um Um = Zero

data Binario = Zero | Um deriving (Show, Eq)
data Funcao = Soma2 | Maior | Menor | Mult2 deriving (Show, Eq)


binToInt :: Binario -> Int
binToInt Zero = 0
binToInt Um   = 1


aplicar :: Funcao -> Binario -> Binario -> Binario
aplicar Soma2 a b = if (binToInt a + binToInt b) `mod` 2 == 1 then Um else Zero
aplicar Maior a b = if binToInt a >= binToInt b then a else b
aplicar Menor a b = if binToInt a <= binToInt b then a else b
aplicar Mult2 a b = if a == Um && b == Um then Um else Zero
-- aplicar Soma2 Um Um = Zero


------------------------------------------------------------------------------------------------------------------


-- 3.12) Faça uma função chamada binList, usando list compreeshion, que recebe uma lista de Binarios (ver exercício anterior) e retorna outra
-- lista com elemento somado Um e convertido para Int. Exemplo:binList [Um, Zero, Zero, Um, Zero] = [0,1,1,0,1]

incBin :: Binario -> Binario
incBin Zero = Um
incBin Um   = Zero

binList :: [Binario] -> [Int]
binList xs = [binToInt (incBin x) | x <- xs]
-- binList [Um, Zero, Zero, Um, Zero] = [0,1,1,0,1]


------------------------------------------------------------------------------------------------------------------


-- 3.13) Faça um novo tipo chamado Metros, que possui um value constructor de mesmo nome, cujos 
-- parâmetros são: um Int que representa a dimensão, e um Double que representa o valor da medida e outro chamado MetragemInvalida. 
-- Implemente as funções:
-- areaQuadrado :: Metros -> Metros: calcula a área de um quadrado.
-- areaRet :: Metros -> Metros -> Metros: calcula a área de um retângulo.
-- areaCubo :: Metros -> Metros: calcula a área de um cubo.

-- Exemplo:
-- Prelude> areaQuadrado (Metros 1 2.0)
-- Metros 2 4.0
-- Use o pattern matching para ignorar as metragens erradas (calcular a área
-- de um quadrado com um lado de dimensão 4 não é válido).

data Metros = Metros Int Double | MetragemInvalida deriving (Show, Eq)

areaQuadrado :: Metros -> Metros
areaQuadrado (Metros 1 v) = Metros 2 (v * v)
areaQuadrado _             = MetragemInvalida

areaRet :: Metros -> Metros -> Metros
areaRet (Metros 1 a) (Metros 1 b) = Metros 2 (a * b)
areaRet _ _                        = MetragemInvalida

areaCubo :: Metros -> Metros
areaCubo (Metros 1 v) = Metros 2 (6 * v * v)
areaCubo _             = MetragemInvalida
-- areaQuadrado (Metros 1 2.0) = Metros 2 4.0


------------------------------------------------------------------------------------------------------------------


-- 3.14) Faça o novo tipo Valido que possui dois value constructors Sim e
-- Nao. O value constructor Sim possui um parâmetro (campo) String.
-- Implemente uma função isNomeValido que recebe um nome e retorna Nao caso
-- a String seja vazia; caso contrário, Sim.

data Valido = Yes String | No deriving (Show, Eq)

isNomeValido :: String -> Valido
isNomeValido "" = No
isNomeValido s  = Yes s


------------------------------------------------------------------------------------------------------------------


-- 3.15) Refaça o exercício 3 do capítulo anterior usando record syntax e
-- tipos com parâmetro (siga o exemplo da conversão de medidas SI para
-- imperial).

data Unidade = Metro | Polegada | Jarda | Pe deriving (Show, Eq)

data Medida = Medida
  { valor   :: Double
  , unidade :: Unidade
  } deriving (Show, Eq)

converterMetrosR :: Medida -> Double
converterMetrosR (Medida v Metro)    = v
converterMetrosR (Medida v Polegada) = v * 0.0254
converterMetrosR (Medida v Jarda)    = v * 0.9144
converterMetrosR (Medida v Pe)       = v * 0.3048

converterImperialR :: Double -> Unidade -> Medida
converterImperialR vEmMetros Metro    = Medida vEmMetros Metro
converterImperialR vEmMetros Polegada = Medida (vEmMetros / 0.0254) Polegada
converterImperialR vEmMetros Jarda    = Medida (vEmMetros / 0.9144) Jarda
converterImperialR vEmMetros Pe       = Medida (vEmMetros / 0.3048) Pe


------------------------------------------------------------------------------------------------------------------


-- 3.16) Faça o tipo Numero, que possui um value constructor Ok com um campo double e outro value constructor Erro com um campo String. 
-- Faça a função dividir que divida dois números e, caso o segundo número seja 0, emita um erro (use o pattern matching). 

-- Exemplo:
-- Prelude> dividir (Numero 6) (Numero 5)
-- Numero 1.2

-- OBS: o enunciado descreve os constructors como "Ok" (double) e "Erro"
-- (String), mas o exemplo usa "Numero 6"/"Numero 5" -- seguem os
-- constructors conforme descritos no texto (Ok/Erro).
data Numero = Ok Double | Erro String deriving (Show, Eq)

dividir :: Numero -> Numero -> Numero
dividir (Erro e) _    = Erro e
dividir _ (Erro e)    = Erro e
dividir (Ok _) (Ok 0) = Erro "Divisao por zero"
dividir (Ok a) (Ok b) = Ok (a / b)
-- dividir (Ok 6) (Ok 5) = Ok 1.2


------------------------------------------------------------------------------------------------------------------


-- 3.17) Faça o tipo Cripto que possua dois values constructors Mensagem e Cifrado, ambos com um campo String 
-- e um value constructor Erro. Faça as funções encriptar e decriptar, seguindo cada exemplo a seguir.

-- Prelude> encriptar (Mensagem "FATEC")
-- Cifrado "GBUFD"
-- Prelude> decriptar (Cifrado "DBTB")
-- Mensagem "CASA"

-- Veja que a encriptação deve empurrar cada letra a frente e a decriptaçãz faz o inverso, empurrando uma letra para trás. 
-- Use as funções succ e pred, e também list compreeshions. Não é possível encriptar mensagens cifradas e decriptar mensagens não cifradas.

data Cripto = Mensagem String | Cifrado String | Error deriving (Show, Eq)

encriptar :: Cripto -> Cripto
encriptar (Mensagem s) = Cifrado [succ c | c <- s]
encriptar _             = Error

decriptar :: Cripto -> Cripto
decriptar (Cifrado s) = Mensagem [pred c | c <- s]
decriptar _            = Error
-- encriptar (Mensagem "FATEC") = Cifrado "GBUFD"
-- decriptar (Cifrado "DBTB")   = Mensagem "CASA"



------------------------------------------------------------------------------------------------------------------



-- 3.18) Faça uma função encriptarTodos que encripta (ou dá erro) todos os elementos de um vetor de Cripto.

encriptarTodos :: [Cripto] -> [Cripto]
encriptarTodos xs = [encriptar x | x <- xs]



------------------------------------------------------------------------------------------------------------------


-- 3.19) Tendo como base o exercício de conversão de medidas, crie uma função que faça conversão de câmbio. 
-- Você deve criar o tipo Cambio contendo os value constructors Euro, Real e Dollar. 
-- Crie também o tip'o Moeda que possui os campos val :: Double e cur :: Cambio.
-- Use record syntax e as taxas de conversão do dia no qual você fez o exercício.

data Cambio = Euro | Real | Dollar deriving (Show, Eq)

data Moeda = Moeda
  { val :: Double
  , cur :: Cambio
  } deriving (Show, Eq)

-- Taxas do dia (19/08/2026, cotacao aproximada de mercado):
-- 1 Dollar = 5.54 Real | 1 Euro = 6.35 Real
taxaDollarParaReal :: Double
taxaDollarParaReal = 5.54

taxaEuroParaReal :: Double
taxaEuroParaReal = 6.35

paraReal :: Moeda -> Double
paraReal (Moeda v Real)   = v
paraReal (Moeda v Euro)   = v * taxaEuroParaReal
paraReal (Moeda v Dollar) = v * taxaDollarParaReal



------------------------------------------------------------------------------------------------------------------



-- 3.20) Crie a função converterTodosReal que recebe uma lista de moedas e retorna outra lista de moedas com todos os seus 
-- elementos convertidos para Real. Use list compreenshion.

converterTodosReal :: [Moeda] -> [Moeda]
converterTodosReal ms = [Moeda (paraReal m) Real | m <- ms]



------------------------------------------------------------------------------------------------------------------


-- 3.21) Crie a função maxMoeda que recebe uma lista de moedas e retorna o valor máximo absoluto (sem conversão alguma)
-- dentre os campos val desta lista.

-- Exemplo:
-- Prelude> maxMoeda [Moeda 3 Real, Moeda 7 Dollar, Moeda 1 Euro]
-- 7

-- Use a função maximum.


maxMoeda :: [Moeda] -> Double
maxMoeda ms = maximum [val m | m <- ms]
-- maxMoeda [Moeda 3 Real, Moeda 7 Dollar, Moeda 1 Euro] = 7.0













