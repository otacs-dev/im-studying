ev :: (Int -> Int) -> Int
ev f = 1 + f 5

dobro :: Int -> Int
dobro x = 2 * x

triplo :: Int -> Int
triplo x = 3 * x

somarTresNum :: Int -> Int -> Int -> Int
somarTresNum x y z = x + y + z

somarCurr :: Int -> Int
somarCurr = somarTresNum 4 5

traseira :: String -> String
traseira [] = []
traseira (x:xs) = xs

contar :: String -> Int
contar = length

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

infixl 9 |>



func :: String -> String
func x = x ++ drop 1 (take 3 (reverse x))

funcI :: String -> String
funcI x = reverse x
          |> take 3
          |> drop 1
          |> (\res -> x ++ res)


imc :: Float -> Float -> String
imc p a
  | p / (a * a) <= 18.5 = "Abaixo do peso"
  | p / (a * a) < 25.0  = "Peso ideal"
  | p / (a * a) <= 30   = "Acima do peso"
  | otherwise           = "Obesidade"
  where
    valorImc = p / (a * a)

fat :: Int -> Int
fat n
  | n <= 1    = 1
  | otherwise = n * fat (n - 1)


reverse' :: String -> String
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
















