data SnocList : List a -> Type where
    Empty : SnocList []
    Snoc : (rec : SnocList xs) -> SnocList (xs ++ [x])

snocListHelp : (snoc : SnocList input) -> (rest : List a) -> SnocList (input ++ rest)
snocListHelp {input} snoc [] = rewrite appendNilRightNeutral input in snoc
snocListHelp {input} snoc (x :: xs) = rewrite appendAssociative input [x] xs in snocListHelp (Snoc snoc {x}) xs

snocList : (xs : List a) -> SnocList xs
snocList xs = snocListHelp Empty xs

myReverseHelper : (input : List a) -> (snoc: SnocList input) -> List a
myReverseHelper [] Empty = []
myReverseHelper (xs ++ [x]) (Snoc rec) = x:: myReverseHelper xs rec


myReverse : List a -> List a
myReverse input = myReverseHelper input (snocList input)


isSuffix : Eq a => List a -> List a -> Bool
isSuffix xs ys with (snocList xs)
  isSuffix [] ys | Empty = True
  isSuffix (xs ++ [x]) ys | (Snoc xsrec) with (snocList ys)
    isSuffix (xs ++ [x]) [] | (Snoc xsrec) | Empty = False
    isSuffix (xs ++ [x]) (ys ++ [y]) | (Snoc xsrec) | (Snoc ysrec) = if x == y then isSuffix xs ys | xsrec | ysrec else False
