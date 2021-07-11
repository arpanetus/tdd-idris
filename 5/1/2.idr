module Main

printLength : IO ()
printLength = putStr "1st string: " >>= \_ =>
              getLine >>= \firstInput =>
              putStr "2nd string: " >>= \_ =>
              getLine >>= \secondInput =>
              putStrLn (show (max (length firstInput) (length secondInput)))


main : IO ()
main = printLength
