import Data.Vect
import Data.Vect.Views


mergeSort : Ord a => Vect len a -> Vect len a
mergeSort input with (splitRec input)
  mergeSort [] | SplitRecNil = []
  mergeSort [x] | SplitRecOne = [x]
  mergeSort (lefts ++ rights) | (SplitRecPair lrec rrec) = merge (mergeSort lefts) (mergeSort rights)


-- dammit, I cheated again and I hate github copilot for making my previous statement wrong :angry: