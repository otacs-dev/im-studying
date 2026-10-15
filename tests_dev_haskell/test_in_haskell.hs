-- runghc test_in_haskell.hs (for run in terminal)
-- ghci test_in_haskell.hs (for terminal - choose the value for "variable")


main :: IO ()
main = print (polinomio 2)

polinomio :: Int -> Int
polinomio x = x*x + 10*x + 2

quadrado :: Int -> Int
quadrado n = n*n

soma :: Float -> Float -> Float
soma a b = a + b

triplica :: Int -> Int
triplica var = var*3


-- !! é uma função falha


