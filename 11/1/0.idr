labelWith : Stream labelType -> List a -> List (labelType, a)
labelWith lbs [] = []
labelWith (lbl :: lbls) (val :: vals) = (lbl, val) :: labelWith lbls vals

label : List a -> List (Integer, a)
label = labelWith (iterate (+1) 0)

quiz : Stream Int -> (score : Nat) -> IO ()
quiz (num1 :: num2 :: nums) score 
    = do putStrLn ("Score so far: " ++ show score)
         putStr (show num1 ++ " * " ++ show num2 ++ "? ")
         answer <- getLine
         if cast answer == num1 * num2 
            then do putStrLn "Correct!"
                    quiz nums (score + 1)
            else do putStrLn ("Wrong, the answer is " ++ show (num1 * num2)) 
                    quiz nums score

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in 
    (seed' `shiftR` 2) :: randoms seed'