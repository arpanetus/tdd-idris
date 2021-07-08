module Main


overLength : Nat -> (List String) -> Nat

overLength l list = length (filter (> l) (map length list))

main : IO ()
main = do
  putStrLn (show (overLength 3 ["One", "Two", "Three", "Four"]))
