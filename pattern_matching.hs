module PatternMatching where

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer
data User = RegisteredUser Username AccountNumber | UnregisteredUser Username

printUser :: User -> IO ()
printUser (UnregisteredUser (Username name)) = putStrLn name
printUser (RegisteredUser (Username name) (AccountNumber num)) = putStrLn $ name ++ " with account " ++ show num

