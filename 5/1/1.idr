module Main

main : IO ()
main = do
  putStr "Input first string: "
  firstInput <- getLine
  putStr "Input second string: "
  secondInput <- getLine
  putStrLn $ show (max (length firstInput) (length secondInput))
