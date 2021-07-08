module Main

import Data.Vect

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = replicate _ []
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                         zipWith (::) x xsTrans



multiMatrix : Num a => Vect l (Vect m a) -> Vect m (Vect n a) -> Vect l (Vect n a)
multiMatrix xs ys = (multiply xs (transposeMat ys))
  where
    dotProduct : Num a => Vect n a -> Vect n a -> a
    dotProduct [] [] = 0
    dotProduct (x :: xs) (y :: ys) = x * y + dotProduct xs ys

    multHelper : Num a => Vect n a -> Vect m (Vect n a) -> Vect m a
    multHelper _ [] = []
    multHelper x (y :: ys) = dotProduct x y :: multHelper x ys

    multiply : Num a => (Vect l (Vect m a)) -> (Vect n (Vect m a)) -> Vect l (Vect n a)
    multiply [] _ = []
    multiply (x::xs) ys = multHelper x ys :: multiply xs ys


main : IO()
main = do
  putStrLn (show (multiMatrix [[1,2], [3,4], [5,6]] [[7,8,9,10], [11,12,13,14]]))
