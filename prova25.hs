import Data.ByteString (group)
fatoresPrimos :: Int -> [(Int, Int)]
fatoresPrimos n = [(head xs, length xs) | xs <- group (factors n 2)]
    where 
        factors 1 d = []
        factors n d 
            | n `mod` d == 0 = d: factors (n `div` d) d
            | otherwise = factors n (d+1)