data InfList : Type -> Type where
    (::) : (value : elem) -> Inf (InfList elem) -> InfList elem

%name InfList xs, ys, zs

Functor InfList where
    map func (value :: xs) = func value :: map func xs 


countFrom : Integer -> InfList Integer
countFrom x = x :: countFrom (x + 1)

getPrefix : Nat -> InfList a -> List a
getPrefix Z x = []
getPrefix (S k) (x :: xs) = x :: getPrefix k xs