module Main where
  import Shape
  main :: IO ()
  main = do
    putStrLn "A circle"
    let c = draw [20, 20] (circle [0,0] 5) [-10, -10]
    putStrLn c

    putStrLn "A ring"
    let c1Sub = SubtractableShape (circle [0, 0] 5)
    let c2Sub = SubtractableShape (circle [0, 0] 8)
    let cSub = c1Sub <> c2Sub
    let sSub = draw [20, 20] cSub [-10, -10]
    putStrLn sSub

    putStrLn "A lens"
    let c1Int = IntersectableShape (circle [0, 0] 8)
    let c2Int = IntersectableShape (circle [6, 0] 8)
    let cInt = c1Int <> c2Int
    let sInt = draw [20, 20] cInt [-10, -10]
    putStrLn sInt

    putStrLn "Myckey Mouse"
    let c1Sum = SummableShape (circle [-5, -5] 4)
    let c2Sum = SummableShape (circle [5, -5] 4)
    let c3Sum = SummableShape (circle [0, 0] 4)
    let cSum = c1Sum <> c2Sum <> c3Sum
    let sSum = draw [20, 20] cSum [-10, -10]
    putStrLn sSum
