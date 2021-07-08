module Main

palindrome : String -> Bool

palindrome str = reverse str == str

palindromeAnswer : Bool -> String
palindromeAnswer res = case res of
  True => "Yes"
  False => "No"

main : IO ()
main = do
  putStrLn (palindromeAnswer ((palindrome "racecar")))
  putStrLn (palindromeAnswer ((palindrome "Racecar")))
