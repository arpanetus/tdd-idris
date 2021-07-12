readToBlank : IO (List String)
readToBlank =  do
  x <- getLine
  if (x == "")
    then pure []
    else do
      xs <- readToBlank
      pure (x::xs)


main : IO()
main = do
  putStrLn "Enter the strings:"
  list <- readToBlank
  putStrLn $ show $ list
