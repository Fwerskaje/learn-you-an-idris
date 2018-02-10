module Ex10

data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double
           
Eq Shape where
  (==) (Triangle x z) (Triangle y w) = x == y && z == w
  (==) (Rectangle x z) (Rectangle y w) = x == y && z == w
  (==) (Circle x) (Circle y) = x == y
  (==) _ _ = False
 
area : Shape -> Double
area (Triangle x y) = 0.5 * x * y
area (Rectangle x y) = x * y
area (Circle x) = pi * x * x

{-

λΠ> Circle 4 == Triangle 3 2
False : Bool

-}

Ord Shape where
  compare x y = compare (area x) (area y)

testShapes : List Shape
testShapes = [Circle 3, Triangle 3 9, Rectangle 2 6, Circle 4,
              Rectangle 2 7]

{-
λΠ> sort testShapes
[Rectangle 2.0 6.0,
 Triangle 3.0 9.0,
 Rectangle 2.0 7.0,
 Circle 3.0,
 Circle 4.0] : List Shape
-}

