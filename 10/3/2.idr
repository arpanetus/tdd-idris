export
data Shape = Triangle Double Double 
           | Rectangle Double Double
           | Circle Double
export
triangle : Double -> Double -> Shape
triangle = Triangle

export
rectangle : Double -> Double -> Shape
rectangle = Rectangle

export
circle : Double -> Shape
circle = Circle


data ShapeView : Shape -> Type where
    STriangle : ShapeView (triangle base height)
    SRectangle : ShapeView (rectangle width height)
    SCircle : ShapeView (circle radius)


shapeView : (shape : Shape) -> ShapeView shape
shapeView (Triangle base height) = STriangle
shapeView (Rectangle width height) = SRectangle
shapeView (Circle radius) = SCircle


area : Shape -> Double
area s with (shapeView s)
    area (triangle base height) | STriangle = base * height / 2
    area (rectangle width height) | SRectangle = width * height
    area (circle radius) | SCircle = pi * radius * radius
