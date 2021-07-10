import Data.Vect
import Data.Fin


sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = case integerToFin pos n of
                              Nothing => Nothing
                              Just idx => Just ((index idx xs) + (index idx ys))



main : IO()
main = do
  putStrLn $ show $ sumEntries 2 [1,2,3,4] [5,6,7,8]
  putStrLn $ show $ sumEntries 4 [1,2,3,4] [5,6,7,8]
