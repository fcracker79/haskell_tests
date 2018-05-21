palindromic' :: String -> Int -> Bool
palindromic' s i
    | len == 0 = True
    | i * 2 >= len = True
    | otherwise = s !! i == s !! (len - i - 1) && palindromic' s (i + 1)
    where len = length s

palindromic :: String -> Bool
palindromic s = palindromic' s 0

