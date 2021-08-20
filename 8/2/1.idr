import Data.Vect

myReverse : Vect n elem -> Vect n elem
myReverse [] = []
myReverse (x :: xs) = reverseProof (myReverse xs ++ [x]) 
    where 
        reverseProof : Vect (k + 1) elem -> Vect (S k) elem
        reverseProof {k} result = rewrite plusCommutative 1 k in result                     



myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m = rewrite plusZeroRightNeutral m in Refl
myPlusCommutes (S k) m = rewrite myPlusCommutes k m in 
                         rewrite plusSuccRightSucc m k in Refl

 -- i don't understand the magic behind the rewrite