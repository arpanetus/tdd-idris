palindrome : String -> Bool

palindrome str = let lowered = toLower str in reverse lowered == lowered

palindromeAnswer : String -> String
palindromeAnswer str = let res = palindrome str in case res of
  True => "Yes\n"
  False => "No\n"

counter : Integer -> String -> Maybe(String, Integer)
counter num "up" = Just (("up: "++(show (num+1))++"\n"), num+1)
counter num "down" = Just (("down: "++(show (num-1))++"\n"), num-1)
counter num  _ = Nothing



repl' : (prompt : String) -> (onInput : String -> String) -> IO ()
repl' prompt onInput = do
  putStr prompt
  input <- getLine
  putStr $ onInput input


replWith' : (state : a) -> (prompt : String) -> (onInput : a -> String -> Maybe (String, a)) -> IO ()
replWith' state prompt onInput = do
  putStr prompt
  input <- getLine
  case onInput state input of
    Just (output, nextState) => do
      putStr output
      replWith' nextState prompt onInput
    Nothing => pure ()


main : IO ()
main = do
  replWith' 0 "up or down:\n" counter
