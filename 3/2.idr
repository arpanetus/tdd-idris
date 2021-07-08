module Main

import Data.Vect

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = (zipWith (+) x y) :: addMatrix xs ys

main : IO()
main = do
  putStrLn (show (addMatrix [[1,2], [3,4]] [[5,6], [7,8]]))
