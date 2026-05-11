import Data.ByteString.Char8 (sort)

oddFunc :: [Int] -> [Int]
oddFunc [] = error "Lista vazia"
oddFunc xs = Sort[x | x <- xs, odd x] 
