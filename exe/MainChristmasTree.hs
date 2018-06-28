module Main where
  import ChristmasTree
  main :: IO ()
  main = do
    putStr "Tree height"
    height <- getLine
    let height_int = read height :: Int
    (printTree . tree) height_int
