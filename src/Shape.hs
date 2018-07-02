module Shape(
    Drawable,
    Shape,
    SummableShape(..),
    SubtractableShape(..),
    IntersectableShape(..),
    Environment,
    (<>),
    draw,
    circle
) where
import Data.Semigroup

type Point = [Double]

class Drawable(a) where
    draw :: Environment -> a -> Point -> String

data Shape = Shape {drawPoint :: Point -> Bool}

data SummableShape = SummableShape Shape
instance Drawable(SummableShape) where
    draw environment (SummableShape (Shape s)) point = draw environment (Shape s) point

data SubtractableShape = SubtractableShape Shape
instance Drawable(SubtractableShape) where
    draw environment (SubtractableShape (Shape s)) point = draw environment (Shape s) point

data IntersectableShape = IntersectableShape Shape
instance Drawable(IntersectableShape) where
    draw environment (IntersectableShape (Shape s)) point = draw environment (Shape s) point

instance Drawable(Shape) where
    draw env s p
        | px > w / 2 || py > h / 2 = ""
        | otherwise = (pointToChar env s p) ++ (draw env s (incr env p))
        where [px, py] = p
              [w, h] = env


type Environment = [Double]

numDiff :: Num a => (a, a) -> a
numDiff x = x1 - x2 where (x1, x2) = x

distance :: Point -> Point -> Double
distance p1 p2 = sqrt (sum (map (^2) (map numDiff (zip p1 p2))))

circle :: Point -> Double -> Shape
circle center radius = Shape (\point -> (distance center point) < radius)


-- From now on points are supposed to be 2D.
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

incr :: Environment -> Point -> Point
incr env p = if px < w / 2 then [(px + 1), py] else [(-w / 2), (py + 1)] where [px, py] = p 
                                                                               [w, h] = env
-- a ring
instance Semigroup(SubtractableShape) where
    (SubtractableShape (Shape s1)) <> (SubtractableShape (Shape s2)) = SubtractableShape $ Shape ( \x -> (s1 x) /= (s2 x) )

-- an intersection of circles
instance Semigroup(IntersectableShape) where
    (IntersectableShape (Shape s1)) <> (IntersectableShape (Shape s2)) = IntersectableShape $ Shape ( \x -> (s1 x) && (s2 x) )

-- Mickey mouse
instance Semigroup(SummableShape) where
    (SummableShape (Shape s1)) <> (SummableShape (Shape s2)) = SummableShape $ Shape ( \x -> (s1 x) || (s2 x) )
