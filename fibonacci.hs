fib :: Int -> [Int]
fib x
    | x == 0 = []
    | x == 1 = [1]
    | x == 2 = [1, 1]
    | otherwise = prev ++ [sum (drop (x - 3) prev)]
    where prev = fib(x - 1)
          
