data ThreeEq: num1 -> num2 -> num3 -> Type where
    Same : ThreeEq num num num

allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS z z z Same = Same

-- god save my damned brain   