module Main

palindrome : String -> Bool

palindrome str = let lowered = toLower str in reverse lowered == lowered

palindromeAnswer : String -> String
palindromeAnswer str = let res = palindrome str in case res of
  True => "Yes\n"
  False => "No\n"

main : IO ()
main = repl "Enter a string: " palindromeAnswer
