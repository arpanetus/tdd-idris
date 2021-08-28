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



halves : List a -> (List a, List a)
halves input = let division = div (length input) 2 in splitByN division input 
    where
    splitByN : (n : Nat) -> (xs : List a) -> (List a, List a)
    splitByN  n xs with (takeN n xs)
        splitByN n xs | Fewer = ([], xs)
        splitByN n (n_xs ++ rest) | (Exact n_xs) = (n_xs, rest)

            
-- pure miracle, I assure you
