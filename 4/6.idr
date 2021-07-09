module Main

data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic pic1) = pictureArea pic + pictureArea pic1
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a

maxMaybe (Just a) (Just b) = case a>b of
                              True => Just a
                              False => Just b
maxMaybe (Just a) Nothing = Just a
maxMaybe Nothing (Just b) = Just b
maxMaybe Nothing Nothing = Nothing


biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive x) = getArea x where
  getArea : Shape -> Maybe Double
  getArea orig@(Triangle base height) = (Just (area(orig)))
  getArea _ = Nothing

biggestTriangle (Combine x y) =
  let xArea = (biggestTriangle x)
      yArea = (biggestTriangle y) in
      maxMaybe xArea yArea

biggestTriangle (Rotate x y) = biggestTriangle y
biggestTriangle (Translate x y z) = biggestTriangle z

-- testPic1 : Picture
-- testPic2 : Picture



main : IO()
main = let testPic1 = Combine (Primitive (Triangle 2 3)) (Primitive (Triangle 2 4))
           testPic2 = Combine (Primitive (Rectangle 1 3)) (Primitive (Circle 4)) in
           do
             putStrLn (show (biggestTriangle testPic1))
             putStrLn (show (biggestTriangle testPic2))
