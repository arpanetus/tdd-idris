import Data.Vect
import Data.Fin


vectTake : {a: Type} -> {m: Nat} -> (n: Nat) -> (xs: Vect (n + m) a) -> Vect n a
