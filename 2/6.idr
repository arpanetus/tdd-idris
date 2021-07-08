module Main

counts : String -> (Nat, Nat)

counts str = ((length (words (str)), (length (str))))

countsAnswer : (Nat, Nat) -> String
countsAnswer (f, s) = cast (f) ++ ", " ++ cast (s)

main : IO ()
main = do
  putStrLn (countsAnswer (counts "Hello, Idris world!"))
