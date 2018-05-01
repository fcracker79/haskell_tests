palindromic :: String -> Bool
palindromic s
    | s == "" = True
    | otherwise = last s == s!!0 && palindromic (take 1 (drop 1 s) )

