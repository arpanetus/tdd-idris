module Main

data Expr : Type -> Type where
     Val : Num elem => (val: elem) -> Expr elem
     Add : Num elem => (left : Expr elem) -> (right: Expr elem) -> Expr elem
     Subt : Neg elem => (left : Expr elem) -> (right: Expr elem) -> Expr elem
     Mult : Num elem => (left : Expr elem) -> (right: Expr elem) -> Expr elem
     Divi : Fractional elem => (left : Expr elem) -> (right: Expr elem) -> Expr elem

evaluate : (Num elem) => Expr elem -> elem
evaluate (Val val) = val
evaluate (Add left right) = (+) (evaluate left) (evaluate right)
evaluate (Subt left right) = (-) (evaluate left) (evaluate right)
evaluate (Mult left right) = (*) (evaluate left) (evaluate right)

-- don't know how to convert elem into Fractional Double
-- evaluate (Divi left right) = (/) (evaluate left) (evaluate right)



main : IO()
main = do
  putStrLn (show (evaluate (Mult (Val 10) (Add (Val 6) (Val 3)))))
