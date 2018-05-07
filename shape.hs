import Data.Semigroup

type Point = [Double]

data Shape = Shape {drawPoint :: Point -> Bool}
type Environment = [Double]

:{
diff :: Num a => (a, a) -> a
diff x = x1 - x2 where (x1, x2) = x
:}

:{
distance :: Point -> Point -> Double
distance p1 p2 = sqrt (sum (map (^2) (map diff (zip p1 p2))))
:}

:{
circle :: Point -> Double -> Shape
circle center radius = Shape (\point -> (distance center point) < radius)
:}


-- From now on points are supposed to be 2D.
:{
pointToChar :: Environment -> Shape -> Point -> String
pointToChar env s p
    | py == h / 2 = "-" ++ cr
    | py == -h / 2 = "-" ++ cr
    | px == w / 2 = "|" ++ cr
    | px == -w / 2 = "|"
    | drawPoint s p = "*"
    | otherwise = " "
    where [px, py] = p
          [w, h] = env
          cr = if px == w / 2 then "\n" else ""
:}

:{
incr :: Environment -> Point -> Point
incr env p = if px < w / 2 then [(px + 1), py] else [(-w / 2), (py + 1)] where [px, py] = p 
                                                                               [w, h] = env
:}

:{
draw :: Environment -> Shape -> Point -> String
draw env s p
    | px > w / 2 || py > h / 2 = ""
    | otherwise = (pointToChar env s p) ++ (draw env s (incr env p))
    where [px, py] = p
          [w, h] = env
:}

s = draw [20, 20] (circle [0,0] 5) [-10, -10]
putStrLn ("A circle\n" ++ s)

-- a ring
:{
instance Semigroup(Shape) where
    (Shape s1) <> (Shape s2) = Shape ( \x -> (s1 x) /= (s2 x) )
:}

c1 = circle [0, 0] 5
c2 = circle [0, 0] 8
c = c1 <> c2
s = draw [20, 20] c [-10, -10]
putStrLn ("A ring\n" ++ s)

-- an intersection of circles
:{
instance Semigroup(Shape) where
    (Shape s1) <> (Shape s2) = Shape ( \x -> (s1 x) && (s2 x) )
:}

c1 = circle [0, 0] 8
c2 = circle [6, 0] 8
c = c1 <> c2
s = draw [20, 20] c [-10, -10]
putStrLn ("An intersection of circles\n" ++ s)

-- Mickey mouse
:{
instance Semigroup(Shape) where
    (Shape s1) <> (Shape s2) = Shape ( \x -> (s1 x) || (s2 x) )
:}
c1 = circle [-5, -5] 4
c2 = circle [5, -5] 4
c3 = circle [0, 0] 4

c = c1 <> c2 <> c3
s = draw [20, 20] c [-10, -10]
putStrLn ("Mickey mouse\n" ++ s)

