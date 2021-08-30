import Data.Primitives.Views

data Face = Heads | Tails

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in (seed' `shiftR` 2) :: randoms seed'

flip : (num: Int) -> Face
flip num with (divides num 2)
  flip ((2 * div) + rem) | (DivBy prf) = 
    case rem of
        0 => Heads
        _ => Tails 

-- the very simple implementation :)
-- flip num = case num `mod` 2 == 0 of
--                 True => Head
--                 False => Tail


coinFlips : (count : Nat) -> Stream Int -> List Face
coinFlips count xs = take count (map flip xs)
