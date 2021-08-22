-- import Data.Vect
data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

-- removeElem : (value : a) -> (xs : Vect (S n) a) -> {auto prf : Elem value xs} -> Vect n a
-- removeElem value (value :: ys) {prf = Here} = ys
-- removeElem {n = Z} value (y :: []) {prf = There later} = absurd later
-- removeElem {n = (S k)} value (y :: ys) {prf = There later} = y :: removeElem value ys


data Elem : a -> Vect k a -> Type where
  Here : Elem x (x :: xs)
  There : (later : Elem x xs) -> Elem x (y :: xs)


notInNil : Elem value [] -> Void
notInNil Here impossible
notInNil (There _) impossible


notInTail : (notThere : Elem value xs -> Void) -> (notHere : (value = x) -> Void) -> Elem value (x :: xs) -> Void
notInTail notThere notHere Here = notHere Refl
notInTail notThere notHere (There later) = notThere later


isElem : DecEq a => (value : a) -> (xs : Vect n a) -> Dec (Elem value xs)
isElem value [] = No notInNil
isElem value (x :: xs) = case decEq value x of
                              (Yes Refl) => Yes Here
                              (No notHere) => (case isElem value xs of
                                                   (Yes prf) => Yes (There prf)
                                                   (No notThere) => No (notInTail notThere notHere))

-- If I could only understand the thing I'm learning rn