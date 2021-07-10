import Data.Vect
import Data.Fin


vectTake : {a: Type} -> {m: Nat} -> (n: Nat) -> (xs: Vect (n + m) a) -> Vect n a
vectTake {a = a} {m = Z} Z [] = []
vectTake {a = a} {m = (S len)} Z (x :: xs) = []
vectTake {a = a} {m = m} (S k) (x :: xs) = x :: vectTake k xs


main : IO()
main = do
  putStrLn $ show $ vectTake 3 [1,2,3,4,5,6,7]
