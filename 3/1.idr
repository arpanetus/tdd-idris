module Main

import Data.Vect

-- If not that link, I wouldn't manage to 'solve'
-- https://timmyjose.github.io/docs/2020-09-01-matrix-operations-in-idris

||| Transpose a matrix
transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = replicate _ []
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                         zipWith (::) x xsTrans

main : IO()
main = do
  putStrLn (show (transposeMat [[1,2], [3,4], [5,6]]))
  putStrLn (show (transposeMat [[7,8], [9,10], [11,12]]))
