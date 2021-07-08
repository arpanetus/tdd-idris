module Main

palindrome : String -> Bool

palindrome str = case ((length str) > 10) of
  False => False
  _ => let lowered = toLower str in reverse lowered == lowered

palindromeAnswer : Bool -> String
palindromeAnswer res = case res of
  True => "Yes"
  False => "No"

main : IO ()
main = do
  putStrLn (palindromeAnswer ((palindrome "racecar")))
  putStrLn (palindromeAnswer ((palindrome "Racecar")))
  putStrLn (palindromeAnswer ((palindrome "Race car")))
  putStrLn (palindromeAnswer ((palindrome "able was i ere i saw elba")))
