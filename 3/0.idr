module Main

import Data.Vect

mlen : Vect n elem -> Nat
mlen [] = 0
mlen (x::xs) = 1 + mlen xs

||| thanks to the lads from telegram funcprog chat and reddit as well:
||| https://www.reddit.com/r/Idris/comments/7zsf07/reverse_function_for_vect/duwq1po/
rev : Vect len elem -> Vect len elem
rev [] = []
rev ((::) {len} x xs) = rewrite plusCommutative 1 len in rev xs ++ [x]


mop : (a -> b) -> List a -> List b
mop f [] = []
mop f (x::xs) = f x :: mop f xs

mop' : (a -> b) -> Vect n a -> Vect n b
mop' f [] = []
mop' f (x::xs) = f x :: mop' f xs
