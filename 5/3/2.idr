readToBlank : IO (List String)
readToBlank =  do
  x <- getLine
  if (x == "")
    then pure []
    else do
      xs <- readToBlank
      pure (x::xs)


compileString : List String -> String
compileString [] = ""
compileString (x :: xs) = x ++ "\n" ++ (compileString xs)


saveToFile : (filename: String) -> (strings: List String) -> IO ()
saveToFile filename strings = do

  result <- writeFile filename (compileString strings)
  case result of
    Left err => do
      putStrLn ("Couldn't write into file " ++ filename)
      pure ()
    Right _ => do
      pure ()


main : IO ()
main = do
  putStrLn "Enter the strings:"
  list <- readToBlank
  putStrLn "Enter the filename:"
  filename <- getLine
  saveToFile filename list
