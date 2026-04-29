maiorEIndice :: (Ord a, Num a) => [a] -> (a, Int)
maiorEIndice [] = error "Lista vazia"
maiorEIndice xs = maximum (zip xs [0..])

{--traduzir :: [Int] -> [String]
traduzir xs = [buscar x dic_10| x <- xs]
    where
        dic_10 = [(0,"zero"), (1,"um"), (2,"dois"), (3,"tres"), (4,"quatro"),
                  (5,"cinco"), (6, "seis"), (7,"sete"), (8,"oito"), (9,"nove")]
        buscar k dict = head [v | (k, v) <- dict, k == k'] --}

del_posicao_n :: [Int] -> Int -> [Int]
del_posicao_n [] _ = error "A lista não possui nenhum elemento"
del_posicao_n xs n = [ x | (x, i) <- zip xs [0..], i /= n]

inserir_posicao_x :: [Int] -> Int -> Int -> [Int]
inserir_posicao_x xs n elem = take n xs ++ [elem] ++ drop n xs

valor_n :: [Int] -> Int -> Int
valor_n [] _ = error "Lista vazia"
valor_n xs n = xs !! n

merge :: (Ord a ) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) 
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys