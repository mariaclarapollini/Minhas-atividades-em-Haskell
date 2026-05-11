import GHC.Base (VecElem(Int16ElemRep))

import System.Win32 (xBUTTON1)
import Distribution.Compat.Lens (_1)
{-1) Como a versão recursiva da função fatorial se comporta se dermos a 
ela como argumento um número negativo? Modifique a implementação clássica 
para não permitir números negativos adicionando uma guarda ao passo recursivo.-}
fatorial :: Int -> Int
fatorial n
    | n < 0  = error "O fatorial de numeros negativos não é calculável"
    | n == 0 = 1
    | otherwise = n * fatorial(n-1)

{-2) Defina a função recursiva somar :: Int -> Int que retorna a soma dos 
inteiros não-negativos a partir de um valor até zero. Por exemplo, somar 3 deve 
retornar 3+2+1+0 = 6.-}

somar :: Int -> Int 
somar a
    | a < 0     = error "Somente inteiros não-negativos são permitidos"
    | a == 0    = 0
    | otherwise = a + somar (a-1)

{-3)Defina o operador de exponenciação ^ utilizando uma função recursiva, 
semelhante ao padrão usado para implementar a multiplicação com o operador *:
(*) :: Num a => a -> a -> a
m * 0 = 0
m * n = m + (m * (n - 1))
 -}

{-4) Defina a função euclides :: Int -> Int -> Int que implementa 
o algoritmo de Euclides para calcular o máximo divisor comum de dois 
inteiros não-negativos: se dois números são iguais, este número é o 
resultado; caso contrário, o menor número é subtraído do maior e o 
processo é repetido passando este novo número e o menor valor passado 
anteriormente como argumento. Exemplo:
> euclides 6 27
3 -}

euclides :: Int -> Int -> Int
euclides a b
    | a < 0 || b < 0 = error "Número negativo não permitido"
    | a == b = a
    | a > b = euclides (a - b) b
    | otherwise = euclides (b - a) a

{-5) Defina as funções abaixo usando recursão:
a) Decidir se todos os valores em uma lista são True-}

verificar :: [Bool] -> Bool
verificar [] = True
verificar (x:xs) = x && verificar xs

{-b) Concatenar uma lista de listas:-}

concatenar :: [[a]] -> [a]
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs

{-c) Produzir uma lista com n elementos idênticos:-}

replicar :: Int -> a -> [a]
replicar n m
    | n <= 0 = []
    | otherwise = m : replicar (n-1) m

{-d)selecionar o n-ésimo elemento de uma lista-}

selecionar :: [a] -> Int -> a
selecionar (x:_) 0 = x
selecionar (_:xs) n = selecionar xs (n-1)

{-e) Decidir se um valor está presente em uma lista-}

elemento :: Eq a => a -> [a] -> Bool
elemento _ [] = error "Lista vazia"
elemento m (x:xs) 
    | m == x = True
    | otherwise = elemento m xs

{-6) Definir a função recursiva merge :: Ord a => [a] -> [a] -> [a] 
que une duas listas ordenadas em uma lista ordenada. Exemplo:
> merge [2,5,6] [1,3,4]
[1,2,3,4,5,6]-}

meuMerge :: Ord a => [a] -> [a] -> [a]
meuMerge [] ys =  ys
meuMerge xs [] = xs
meuMerge (x:xs) (y:ys) 
    | x <= y = x : meuMerge xs (y:ys)
    | otherwise = y : meuMerge (x:xs) ys

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
