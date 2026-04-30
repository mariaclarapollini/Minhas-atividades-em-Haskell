import System.Win32 (COORD(xPos))
{- HLINT ignore "Use camelCase" -}
{- 1) Fornecidos três valores a, b e c, 
escreva uma função que retorne quantos dos três são iguais. 
A resposta pode ser 3 (todos iguais), 2 (dois iguais e o terceiro diferente) 
ou 0 (todos diferentes).-}

funcaoIguais :: Int -> Int -> Int -> Int
funcaoIguais a b c
    | a == b && b == c           = 3
    | a == b || a == c || b == c = 2
    | otherwise                  = 0

{-Fornecidos três valores a, b e c, elaborar uma função que retorne 
quantos desses três valores são maiores que a média entre eles.-}

valorMaiorMedia :: Double -> Double -> Double ->Double
valorMaiorMedia a b c
    | a > media && b > media && c > media = 3
    | (a > media && b > media) || (c > media && b > media) || (a > media && c > media)= 2
    | a > media || b > media || c > media = 1
    | otherwise = 0
    where
        media = (a + b + c) / 3

{-Escreva uma função potencia_2 que retorne o quadrado de um número (x2).-}

potencia_2 :: Int -> Int
potencia_2 x = x * x

{-Reutilizando a função potencia_2, construir uma função potencia_4 
que retorne o seu argumento elevado à quarta potência.-}

potencia_4 :: Int -> Int
potencia_4  = (potencia_2 . potencia_2)

{- Implemente em Haskell a função do ou-exclusivo, que é dada por:ab=(ab) (ab)-}

ou_exclusivo :: Bool -> Bool -> Bool
ou_exclusivo a b = (a || b) && not (a && b)

r1 :: (Floating a, Ord a) => a -> a -> a -> a
r1 a b c = (-b + sqrt (b^2-4*a*c))/(2*a)
r2 :: (Floating a, Ord a) => a -> a -> a -> a
r2 a b c = (-b - sqrt (b^2-4*a*c))/(2*a)

maior_x :: (Floating a, Ord a) => a -> a -> a -> a
maior_x a b c
    | resultado1 > resultado2   = resultado1
    | otherwise = resultado2
    where
        resultado1 = r1 a b c
        resultado2 = r2 a b c

menor_x ::(Floating a, Ord a) => a -> a -> a -> a
menor_x a b c
    | resultado1 < resultado2    = resultado1
    | otherwise = resultado2
    where
        resultado1 = r1 a b c
        resultado2 = r2 a b c

{--Criar funções que calculam a soma dos números entre n1 e n2, 
incluindo e excluindo os limites. --}

somaExclusive :: Int -> Int -> Int
somaExclusive a b = sum [a+1..b-1]

{--9) Dados três números n1, n2 e n3, encontrar os múltiplos de 
n3 que se encontram no intervalo [n1,n2] (inclusivo).--}

elementos :: Int -> Int -> Int -> [Int]
elementos a b c  = [x | x <- [a..b], mod x c == 0]

{--10) Implemente a função mod2, que retorna o resto de uma divisão de inteiros.
OBS: não é permitido usar a função mod nem a função rem da biblioteca.--}

mod2 :: Int -> Int -> Int
mod2 x y
    |x < y     = x
    |otherwise = mod2 (x - y) y

{-Seja a sequência:
a1=sqrt6
a2=sqrt(6+sqrt6)
a3=sqrt(6+sqrt6)
a4=...
Escreva a função que calcula essa sequência.
-}
sequencia :: Int -> Double
sequencia 1 = sqrt 6
sequencia n = sqrt (6 + sequencia (n-1))

{-Implementar a fórmula que indica de quantas maneiras 
é possível escolher n objetos de uma coleção original de m objetos, onde m >= n.-}

{-Fórmula de combinação simples-}

fatorial :: (Integral a) => a -> a
fatorial x
    | x < 0      = error "Fatorial negativo"
    | x == 0     = 1
    | otherwise  = x * fatorial (x-1)


combinacao :: (Integral a) => a -> a -> a
combinacao m n 
    | n < 0 || m >= n = error "Fatorial negativo" 
    | otherwise = fatorial m `div` (fatorial n * fatorial (m-n))

{--Defina uma função que, dada uma lista numérica, retorne uma tupla,
que contenha o maior da lista bem como índice na lista.--}

maiorEIndice :: (Ord a, Num a) => [a] -> (a, Int)
maiorEIndice [] = error "Lista vazia"
maiorEIndice xs = maximum (zip xs [0..])
{----}
traduzir :: [Int] -> [String]
traduzir xs = [buscar x dic_10| x <- xs]
    where
        dic_10 = [(0,"zero"), (1,"um"), (2,"dois"), (3,"tres"), (4,"quatro"),
                  (5,"cinco"), (6, "seis"), (7,"sete"), (8,"oito"), (9,"nove")]
        buscar k dict = head [v | (k', v) <- dict, k == k'] 

{--Construa uma função del_posicao_n :: [Int] -> Int -> [Int] em 
que dada uma lista de inteiros e a posição de um elemento qualquer, 
retorne uma nova lista sem aquele elemento da n-ésima posição.
--}
del_posicao_n :: [Int] -> Int -> [Int]
del_posicao_n [] _ = error "A lista não possui nenhum elemento"
del_posicao_n xs n = [ x | (x, i) <- zip xs [0..], i /= n]

{--Construa uma função inserir_posicao_x :: [Int] -> Int -> Int -> [Int] em que, 
dada uma lista de inteiros, uma posição e um elemento a ser inserido, 
retorne uma nova lista com aquele elemento na n-ésima posição.
--}
inserir_posicao_x :: [Int] -> Int -> Int -> [Int]
inserir_posicao_x xs n elem = take n xs ++ [elem] ++ drop n xs

{--Defina uma função que retorne o valor da n-ésima posição de uma lista.--}
valor_n :: [Int] -> Int -> Int
valor_n [] _ = error "Lista vazia"
valor_n xs n = xs !! n
{-Dadas duas listas ordenadas como entrada, faça uma função merge, que une as duas listas.--}
merge :: (Ord a ) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) 
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

{--Implemente uma função que receba duas listas de inteiros sem repetição, 
e retorne uma terceira lista que contenha somente números que estejam nas duas listas dadas..
--}

funcao_repete :: [Int] -> [Int] -> [Int]
funcao_repete xs ys = [x | x <- xs, x `elem` ys]

{--Crie uma função que faça uma compressão sobre uma sequência de caracteres 
iguais, substitua a sequência por !na, onde n é o número de vezes que o 
caractere a é repetido. Observe que só devem ser comprimidas sequências de 
tamanhos maiores que 3. Exemplo:--}

comprimir :: (String) -> (String)
contar :: (String) -> [(Char, Int)]

contar [] = []
contar (x:xs) =
    (x, qt) : contar resto
    where
        (iguais, resto) = span (== x) xs
        qt = 1 + lenght iguais
comprimir s = concat [if n >= 4 then "!" ++ show n ++ [c] else replicate n c | (c, n) <- contar s]
