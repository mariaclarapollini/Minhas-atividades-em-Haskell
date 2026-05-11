import Text.Printf
{-1. Considere uma lista de tuplas, onde cada uma delas armazena o nome de 
um funcionário como primeiro valor e uma lista de nomes de seus dependentes
como o segundo valor. Crie uma função que retorne uma lista com todos os 
dependentes de todos os funcionários.

Ex: Fornecendo a lista [("José", ["Filho1", "Filho2"]), ("Maria", 
["Filho3"]), ("Carlos", ["Filho4", "Filho5", "Filho6"])] a função 
deve retornar a lista ["Filho1", "Filho2", "Filho3", "Filho4", "Filho5", 
"Filho6"].-}

listaTodos :: [(String, [String])] -> [String]
listaTodos [] = error "Lista vazia"
listaTodos funcionarios = [dep | (_, dep) <- funcionarios, dep <- dep]

{-2. Crie uma função que remova o menor elemento de uma lista. 
Caso este elemento apareça mais de uma vez na lista, remova 
apenas a primeira ocorrência.
Ex: removeMin [4,5,6,4,7] = [5,6,4,7]-}

removeMin :: [Int] -> [Int]
removeMin [] = error "Lista vazia"
removeMin [x] = []
removeMin xs = remove (minimum xs) xs 
    where
        remove _ [] = []
        remove m (y:ys) 
            | m == y = ys 
            | otherwise = y : remove m ys

{-3. (2,5 pontos) Crie uma função que converta uma quantidade 
de segundos em uma string no formato "HH.MM.SS". O valor máximo 
possível a ser recebido por essa função é 359999 (99:59:59).
Ex: converter 86399 = "23:59:59"-}

converter :: Int -> String
converter n = printf "%02d:%02d:%02d" h m s
    where 
        h = n `div` 3600
        m = (n `mod` 3600) `div` 60
        s = n `mod` 60


{-4. (2,5 pontos) Crie uma função (inclua a definição do seu tipo) que 
recebe uma lista de funções e um segundo argumento. Esta função devolve 
uma lista com todas as funções que ao serem aplicadas ao argumento 
retornam False.
Ex: escolheFuncoes [even, odd, (\x -> x mod 3 == 0)] 6 = [odd]-}

wichFalse :: [a -> Bool] -> a -> [a -> Bool]
wichFalse fs arg = [f | f <- fs, not (f arg)]