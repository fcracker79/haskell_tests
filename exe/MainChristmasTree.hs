module Main where
  import ChristmasTree
  main :: IO ()
  main = (printTree . tree) 4
