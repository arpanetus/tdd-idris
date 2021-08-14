-- import Data.Vect

data Vect : (len : Nat) -> (elem : Type) -> Type where
    Nil  : Vect Z elem
    (::) : (x : elem) -> (xs : Vect len elem) -> Vect (S len) elem



Foldable (Vect len) where
    foldr func acc [] = acc
    foldr func acc (x :: xs) = func x (foldr func acc xs)
    foldl func acc [] = acc
    foldl func acc (x :: xs) = foldl func (func acc x) xs


Eq elem => Eq (Vect len elem) where
    (==) [] [] = True
    (==) (x :: xs) (y :: ys) = x == y && (==) xs ys