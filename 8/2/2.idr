import Data.Vect


reverseProof_nil : (acc : Vect n a) -> Vect (plus n 0) a
reverseProof_nil {n} xs = rewrite plusZeroRightNeutral n in xs

reverseProof_xs : Vect ((S n) + len) a -> Vect (plus n (S len)) a
reverseProof_xs {n} {len} xs = rewrite sym (plusSuccRightSucc n len) in xs

reverse' : Vect n a -> Vect m a -> Vect (n+m) a
reverse' acc [] = reverseProof_nil acc
reverse' acc (x :: xs) = reverseProof_xs (reverse' (x::acc) xs) 

myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs  
