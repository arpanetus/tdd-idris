module Main


topTen : (Ord a) => List a -> List a
topTen list = take 10 (reverse ( sort list))

main : IO ()
main = do
  putStrLn (show (topTen [1..100]))
