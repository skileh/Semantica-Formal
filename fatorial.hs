-- exemplo de funcao fatorial em haskell

fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n-1)

