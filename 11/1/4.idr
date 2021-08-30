squareRootApprox : (number : Double) -> (approx : Double) -> Stream Double
squareRootApprox number approx = let next = (approx + (number / approx)) / 2 in approx :: squareRootApprox number next

squareRootBound : (max : Nat) -> (number : Double) -> (bound : Double) -> (approxs : Stream Double) -> Double
squareRootBound Z number bound (value :: xs) = value
squareRootBound (S k) number bound (value :: xs) = 
    if abs(value*value - number) < bound 
        then value
        else squareRootBound k number bound xs


squareRoot : (number : Double) -> Double
squareRoot number = squareRootBound 100 number 0.00000000001 (squareRootApprox number number)