import GHC.Base (VecElem(Int16ElemRep))

meuMerge :: Ord a => [a] -> [a] -> [a]
meuMerge [] ys = ys
meuMerge xs [] = xs
meuMerge (x:xs) (y:ys)
    | x <= y = x : meuMerge xs (y:ys)
    | otherwise = y: meuMerge ys (x:xs)

{-7) Usando a função merge, defina a função mergesort :: Ord a => [a] -> [a] 
que implementa o algoritmo de ordenação merge sort, que por sua vez considera 
uma lista vazia e uma lista com apenas um elemento como listas ordenadas, e que 
qualquer outra lista é ordenada a partir da união de duas listas que resultaram 
da ordenação das suas metades separadamente. Dica: primeiro implemente a função 
metades :: [a] -> ([a],[a]) que separa uma lista em duas partes cujos comprimentos 
são iguais ou no máximo diferem em uma unidade.-}
metades :: [a] -> ([a], [a])
metades xs = (take n xs, drop n xs)
    where 
        n = length xs `div` 2

meuMergeSort :: Ord a => [a] -> [a]
meuMergeSort [] = []
meuMergeSort [x] = [x]
meuMergeSort xs = meuMerge (meuMergeSort esquerda) (meuMergeSort direita)
    where 
        (esquerda, direita) = metades xs

{-8) Implemente recursivamente funções que:
a) calcule a soma de uma lista de inteiros;-}

soma :: [Int] -> Int
soma [] = 0
soma (x:xs) = x + soma xs

{-b ) obtenha o número de elementos de uma lista-}

meuLenght :: [a] -> Int
meuLenght [] = 0
meuLenght (x:xs) = 1 + meuLenght xs 

{-c) selecione o último elemento de uma lista não vazia-}

meuInit :: [a] -> a
meuInit [] = error "Lista vazia"
meuInit [x] = x
meuInit (x:xs) = meuInit xs