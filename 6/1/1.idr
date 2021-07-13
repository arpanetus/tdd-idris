import Data.Vect

Matrix : Nat -> Nat -> Type
Matrix n m = Vect n (Vect m Nat)


testMatrix : Matrix 2 3
testMatrix = [[0, 0, 0], [0, 0, 0]]
