data Shape = Triangle Double Double
    | Rectangle Double Double
    | Circle Double


area : Shape -> Double
area (Triangle x y) = x*y/2
area (Rectangle x y) = x*y
area (Circle x) = 3.1415*x*x



Eq Shape where
    (==) x y = area x == area y

Ord Shape where
    compare x y = compare (area x) (area y)

testShapes : List Shape
testShapes = [Circle 3, Triangle 3 9, Rectangle 2 6, Circle 4, Rectangle 2 7]
