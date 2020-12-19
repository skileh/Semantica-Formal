somaLista :: [Int] -> Int
somaLista []   = 0
somaLista (a:x) = a+somaLista x

dobraLista :: [Int] -> [Int]
dobraLista [] = []
dobraLista (x:xs) = 2 *x : dobraLista xs

tamanho :: [Int] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

produtoLista :: [Int] -> Int
produtoLista [] = 1
produtoLista (x:xs) = x * produtoLista xs

andLista :: [Bool] -> Bool
andLista [True] = True
andLista [False] = False
andLista (x:xs) = x && andLista xs

concatList :: [[Int]] -> [Int]
concatList [] = []
concatList (x:xs) = x ++ concatList xs  

inverte :: [Int] -> [Int]
inverte [] = []
inverte (x:xs) = inverte xs ++ (x:[])
