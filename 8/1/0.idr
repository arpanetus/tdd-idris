data Vect : Nat -> Type -> Type where
    Nil : Vect Z a
    (::) : a -> Vect k a -> Vect (S k) a

                    
data EqNat : (num1 : Nat) -> (num2 : Nat) -> Type where
    Same : (num : Nat) -> EqNat num num
                        


sameS : (k : Nat) -> (j: Nat) -> (eq : EqNat k j) -> EqNat (S k) (S j)
sameS k k (Same k) = Same (S k)

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
checkEqNat Z Z = Just Refl
checkEqNat (S k) Z = Nothing
checkEqNat Z (S k) = Nothing
checkEqNat (S k) (S j) = case checkEqNat k j of
                              Nothing => Nothing
                              (Just prf) => Just (cong prf)

exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength {m} len input = case checkEqNat m len of
                                    Nothing => Nothing
                                    (Just Refl) => Just input
             