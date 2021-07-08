module Main

palindrome : Nat -> String -> Bool

palindrome num str = case ((length str) > num) of
  False => False
  _ => let lowered = toLower str in reverse lowered == lowered

palindromeAnswer : Bool -> String
palindromeAnswer res = case res of
  True => "Yes"
  False => "No"

main : IO ()
main = do
  putStrLn (palindromeAnswer ((palindrome 10 "racecar")))
  putStrLn (palindromeAnswer ((palindrome 5 "Racecar")))
  putStrLn (palindromeAnswer ((palindrome 1 "Race car")))
  putStrLn (palindromeAnswer ((palindrome 15 "able was i ere i saw elba")))
