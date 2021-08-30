data TakeN : List a -> Type where
    Fewer : TakeN xs
    Exact : (n_xs : List a) -> TakeN (n_xs ++ rest)

total
takeN : (n : Nat) -> (xs : List a) -> TakeN xs
takeN n xs = takeNHelper n xs
    where
        takeNHelper : (n: Nat) ->(input: List a) -> TakeN input
        takeNHelper Z [] = Fewer
        takeNHelper Z input = Exact []
        takeNHelper (S k) [] = Fewer
        takeNHelper (S k) (x :: xs) = 
            case takeNHelper k xs of
                Fewer {xs} => Fewer
                Exact n_xs => Exact (x::n_xs)


groupByN : (n : Nat) -> (xs : List a) -> List (List a)
groupByN n xs with (takeN n xs)
    groupByN n xs | Fewer = [xs]
    groupByN n (n_xs ++ rest) | (Exact n_xs) = n_xs :: groupByN n rest

-- so far this one was solved purely by myself, I HAVE NO IDEA THE WAY IT WORKS