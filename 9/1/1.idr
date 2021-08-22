data MList : (elem: Type) -> Type where
  Nil : MList elem
  (::) : (x: elem) -> (xs: MList elem) -> MList elem


data Elem : a -> MList a -> Type where
  Here : Elem x (x :: xs)
  There : (later : Elem x xs) -> Elem x (y :: xs)
