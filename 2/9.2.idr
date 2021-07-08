module Main

counts : String -> (Nat, Nat)

counts str = ((length (words (str)), (length (str))))

countsAnswer : String -> String
countsAnswer str = show (counts str) ++ "\n"

main : IO ()
main = repl "Enter a string: " countsAnswer
