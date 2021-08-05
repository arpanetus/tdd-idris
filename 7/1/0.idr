data Matter = Solid | Liquid | Gas

Eq Matter where
    (==) Solid Solid = True
    (==) Liquid Liquid = True
    (==) Gas Gas = True
    (==) _ _ = False

    (/=) x y = not (x == y)

occurrences : (Eq x) => x -> (List x) -> Nat
occurrences el [] = 0
occurrences el (x::xs) = case el==x of 
    True => 1 + occurences el xs
    False => occurences el xs

