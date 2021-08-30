import Data.List.Views

equalSuffix : Eq a => List a -> List a -> List a
equalSuffix xs ys = equalSuffixWithAcc xs ys [] where
    equalSuffixWithAcc : Eq a => List a -> List a -> List a -> List a
    equalSuffixWithAcc xs ys acc with (snocList xs)
      equalSuffixWithAcc [] ys acc | Empty = acc
      equalSuffixWithAcc (xs ++ [x]) ys acc | (Snoc xsrec) with (snocList ys)
        equalSuffixWithAcc (xs ++ [x]) [] acc | (Snoc xsrec) | Empty = acc
        equalSuffixWithAcc (xs ++ [x]) (ys ++ [y]) acc | (Snoc xsrec) | (Snoc ysrec) 
            = if x == y 
                 then equalSuffixWithAcc xs ys (x::acc) | xsrec | ysrec 
                 else acc