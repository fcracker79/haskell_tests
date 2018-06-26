module MySum where

sum1 :: (Num a, Eq a) => [a] -> a
sum1 arr
    | arr == [] = 0
    | otherwise = arr!!0 + sum1(drop 1 arr)

sum2 :: Num a => [a] -> a
sum2 [] = 0
sum2 (x0:xs) = x0 + sum2 xs
