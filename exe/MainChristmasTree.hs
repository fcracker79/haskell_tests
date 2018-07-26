module Main where
  import ChristmasTree
  import System.IO
  main :: IO ()
  main = do
    putStr "Tree height: "
    hFlush stdout
    height <- getLine
    let height_int = read height :: Int
    (printTree . tree) height_int
