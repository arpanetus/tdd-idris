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
    -- show (Equal x y) = (show x) ++ " == " ++ (show y)

    

eval : (Neg elem, Integral elem, Abs elem) => Expr elem -> elem
eval (Val val) = val
eval (Add left right) = (eval left) + (eval right)
eval (Sub left right) = (eval left) - (eval right)
eval (Mul left right) = (eval left) * (eval right)
eval (Div left right) = (eval left) `div` (eval right)
eval (Abs x) = abs (eval x)

(Eq ty, Neg ty, Integral ty, Abs ty) => Eq (Expr ty) where
    (==) x y = (eval x) == (eval y)
