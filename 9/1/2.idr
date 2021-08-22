
-- data Elem : a -> Vect k a -> Type where
--   Here : Elem x (x :: xs)
--   There : (later : Elem x xs) -> Elem x (y :: xs)


-- notInNil : Elem value [] -> Void
-- notInNil Here impossible
-- notInNil (There _) impossible


-- notInTail : (notThere : Elem value xs -> Void) -> (notHere : (value = x) -> Void) -> Elem value (x :: xs) -> Void
-- notInTail notThere notHere Here = notHere Refl
-- notInTail notThere notHere (There later) = notThere later


-- isElem : DecEq a => (value : a) -> (xs : Vect n a) -> Dec (Elem value xs)
-- isElem value [] = No notInNil
-- isElem value (x :: xs) = case decEq value x of
--                               (Yes Refl) => Yes Here
--                               (No notHere) => (case isElem value xs of
--                                                    (Yes prf) => Yes (There prf)
--                                                    (No notThere) => No (notInTail notThere notHere))

data Last : List a -> a -> Type where
  LastOne : Last [value] value
  LastCons : (prf : Last xs value) -> Last (x :: xs) value

notInNil : Last [] value -> Void
notInNil LastOne impossible
notInNil (LastCons _) impossible

notInLast : (contra : (y = value) -> Void) -> Last [y] value -> Void
notInLast contra LastOne = contra Refl
notInLast _ (LastCons LastOne) impossible
notInLast _ (LastCons (LastCons _)) impossible


notInCons : (contra : Last (x :: xs) value -> Void) -> Last (y :: (x :: xs)) value -> Void
notInCons contra (LastCons prf) = contra prf

isLast : DecEq a => (value : a) -> (xs : List a) -> Dec (Last xs value)
isLast value [] = No notInNil
isLast value (y::[]) = case decEq y value of
                            (Yes Refl) => Yes LastOne
                            (No contra) => No (notInLast contra)

isLast value (y::(x::xs)) = case isLast value (x::xs) of
                                (Yes prf) => Yes (LastCons prf)
                                (No contra) => No (notInCons contra)


-- I'm just cheating from Edwin's repo :<