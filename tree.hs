module Tree where

type Space = Int
type Star = Int
type Layer = (Space, Star)

treeLayers :: Layer -> [Layer]
treeLayers layer
    | spaces == 0 = [(spaces, stars)]
    | otherwise = [(spaces, stars)] ++ (treeLayers((spaces - 1, stars + 2)))
    where (spaces, stars) = layer

layerToText :: Layer -> String
layerToText layer = concat ((replicate spaces " ") ++ (replicate stars "*")) where (spaces, stars) = layer

christmasTreeArray :: Int -> [String]
christmasTreeArray 0 = []
christmasTreeArray height = map layerToText (treeLayers( (height - 1, 1) ))

concatLines :: [String] -> String
concatLines n
    | n == [] = ""
    | otherwise = n!!0 ++ "\n" ++ (concatLines (drop 1 n))

printChristmasTree :: Int -> IO()
printChristmasTree = putStr . concatLines . christmasTreeArray
