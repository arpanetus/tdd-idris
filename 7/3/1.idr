module Main

data Expr num = Val num 
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
              | Abs (Expr num)
            --   | Equal (Expr num) (Expr num)

Num ty => Num (Expr ty) where
    (+) = Add
    (*) = Mul
    fromInteger = Val . fromInteger

Neg ty => Neg (Expr ty) where
    negate x = 0 - x
    (-) = Sub

Abs ty => Abs (Expr ty) where
    abs = Abs

Show ty => Show (Expr ty) where
    show (Val x) = show x
    show (Add x y) = "(" ++ (show x) ++ " + " ++ (show y) ++ ")"
    show (Sub x y) = "(" ++ (show x) ++ " - " ++ (show y) ++ ")"
    show (Mul x y) = "(" ++ (show x) ++ " * " ++ (show y) ++ ")"
    show (Div x y) = "(" ++ (show x) ++ " / " ++ (show y) ++ ")"
    show (Abs x) = "|" ++ (show x) ++"|"

    

eval : (Neg elem, Integral elem, Abs elem) => Expr elem -> elem
eval (Val val) = val
eval (Add left right) = (eval left) + (eval right)
eval (Sub left right) = (eval left) - (eval right)
eval (Mul left right) = (eval left) * (eval right)
eval (Div left right) = (eval left) `div` (eval right)
eval (Abs x) = abs (eval x)

(Eq ty, Neg ty, Integral ty, Abs ty) => Eq (Expr ty) where
    (==) x y = (eval x) == (eval y)

(Neg elem, Integral elem, Abs elem) => Cast (Expr elem) elem where
    cast x = eval x
    

    
Functor Expr where
    map func (Val x) = Val (func x)
    map func (Add x y) = Add (map func x) (map func y) 
    map func (Sub x y) = Sub (map func x) (map func y)
    map func (Mul x y) = Mul (map func x) (map func y)
    map func (Div x y) = Div (map func x) (map func y)
    map func (Abs x) = Abs (map func x)

    