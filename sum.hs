somma :: [Integer] -> Integer
somma arr
    | arr == [] = 0
    | otherwise = arr!!0 + somma(drop 1 arr)

somma [] = 0
somma (x0:xs) = x0 + somma xs
