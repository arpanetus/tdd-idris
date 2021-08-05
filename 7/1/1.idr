data Shape = Triangle Double Double
    | Rectangle Double Double
    | Circle Double


Eq Shape where
    (==) (Circle x) (Circle y) = x == y
    (==) (Rectangle x y) (Rectangle w z) = x == w && y == z
    (==) (Triangle x y) (Triangle w z) = x == w && y == z
    (==) _ _ = False
