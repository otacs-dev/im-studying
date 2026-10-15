--          Area do círculo
area :: Float -> Float
area raio = raio*raio*pi

--            Hipotenusa 
-- sqrt é a raiz da soma desses produtos (a*a + b*b)
hipotenusa :: Float -> Float -> Float
hipotenusa a b = sqrt (a*a + b*b)


--     Diferença da area de dois circulos
-- abs é apenas pra calcular o valor absoluto, 
--se for negativo vira positivo, cuidado!
diferenca :: Float -> Float -> Float
diferenca r1 r2 = abs((area r1) - (area r2))

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++

--                   Funçoes em Haskell

-- abs --> retorna valor absoluto
-- ceiling --> retorna o menor inteiro não menor que o argumento
-- cos --> retorna o cosseno de um número descrito em radianos
-- div --> retorna o quociente inteiro da divisão de dois inteiros 
-- exp --> retorna o valor correspondente a e^n
-- mod --> usado pra achar o resto da divisão de dois inteiros
-- floor --> contrário de ceiling, retorna o maior inteiro não menor que o argumento
-- lcm --> retorna o mínimo múltiplo comum entre dois inteiros
-- pi --> retorna o valor de pi
-- round --> retorna o inteiro mais próximo do argumento
-- sin --> retorna o seno de um número descrito em radianos
-- sqrt --> retorna a raíz quadrada do argumento
-- tan --> retorna a tangente de um número descrito em radianos
-- truncate --> retorna o inteiro sem parte fracionada do número


--                 Operadores em Haskell

-- > --> maior que
-- >= --> maior ou igual
-- < --> menor que
-- <= --> menor ou igual
-- /= --> diferente

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++
