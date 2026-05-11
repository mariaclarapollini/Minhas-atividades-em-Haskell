import Text.Printf
{-1ª QUESTÃO (2,5 pontos): Implemente a função reverso :: [a] -> [a], 
que inverte a ordem dos elementos de uma lista. OBS: Não é permitido 
usar a função reverse da biblioteca padrão.
Ex: reverso "abc"
Main> "cba"-}

reverso :: [a] -> [a]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]

{- 2ª QUESTÃO (2,5 pontos): Crie uma função que converta 
uma quantidade de segundos em uma String no formato "HH:MM:SS". 
O valor máximo possível a ser recebido por essa função é 359999 
("99:59:59").
Ex: converter 86399 = "23:59:59"-}

converter :: Int -> String
converter n = printf "%02d:%02d:%02d" h m s
    where
        h = n `div` 3600
        m = (n `mod` 3600) `div` 60
        s = n `div` 60

{-3ª QUESTÃO (2,5 pontos): Implemente uma função chamada 
rotateLeft :: Int -> [a] -> [a], que rotaciona uma lista 
para a esquerda uma quantidade n de vezes de acordo com o primeiro 
parâmetro da função.
Ex: rotateLeft 2 "abcd"
Main> "cdab"-}

rotateLeft :: Int -> [a] -> [a]
rotateLeft _ [] = []


{-4ª QUESTÃO (2,5 pontos): Crie uma função que remova o menor elemento de uma lista. Caso este elemento apareça mais de uma vez na lista, remova apenas a primeira ocorrência.
Ex: removeMin [4,5,6,4,7] = [5,6,4,7]-}

removeMin :: [Int] -> [Int]
removeMin [] = error "Lista vazia"
removeMin [x] = []
removeMin xs = remover (minimum xs) xs
    where
        remover _ [] = []
        remover m (y:ys)
            | m == y = ys
            | otherwise = y : remover m ys
