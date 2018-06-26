module ChristmasTree where


data TreeLayer = TreeLayer Int Int
type Tree = [TreeLayer]

instance Show TreeLayer where
    show (TreeLayer spaces stars) = concat ((replicate spaces " ") ++ (replicate stars "*"))


tree' :: TreeLayer -> [TreeLayer]
tree' (TreeLayer spaces stars)
  | stars <= 0 = []
  | otherwise = (tree' previousLayer) ++ [TreeLayer spaces stars]
  where previousLayer = TreeLayer (spaces + 1) (stars - 2)


tree :: Int -> Either String Tree
tree h
  | h <= 0 = Left "Invalid height"
  | otherwise = Right $ tree' $ TreeLayer noSpaces baseStars where baseStars = 2 * h - 1; noSpaces = 0

printTree :: Either String Tree -> IO ()
printTree (Right a_tree) = putStrLn $ concat $ map (\x -> show x ++ "\n") a_tree
printTree (Left errorMessage) = putStrLn errorMessage
