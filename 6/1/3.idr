-- TupleVect
TupleVect : Nat -> Type -> Type
TupleVect Z _ = ()
TupleVect (S k) a = (a, TupleVect k a)

test : TupleVect 4 Nat
test = (1,2,3,4,())

-- answer has been ripped off from:
-- https://stackoverflow.com/questions/64194228/smart-constructor-for-tuple-in-idris
