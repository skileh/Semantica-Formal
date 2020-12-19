-- Exercicios da lista 2 sobre Haskell

maxx :: Int -> Int -> Int 
maxx x y 
  | x>=y = x
  | otherwise = y


venda :: Int -> Int
venda 0 = 10
venda 1 = 20
venda 2 = 30
venda 4 = 40
venda _ = 50

maiorVenda :: Int -> Int 
maiorVenda 0 = 
maiorVenda n =  