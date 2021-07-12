import Data.Vect


readVect : (f: File) ->  IO (n ** Vect n String)
readVect f = do
  result <- fGetLine f
  case result of
    Left err => do
      putStrLn ("Couldn't read the line of file: " ++ (show err))
      pure (_ ** [])
    Right x =>
      do if (x == "")
        then pure (_ ** [])
        else do
          (_ ** xs) <- readVect f
          pure (_ ** x :: xs)


readVectFile : (filename: String) ->  IO (n ** Vect n String)
readVectFile filename = do
  result <- openFile filename Read
  case result of
    Left err => do
      putStrLn ("Couldn't open file: " ++ (show err))
      pure (_ ** [])
    Right file => do
      readVect file



main : IO ()
main = do
  putStrLn "Enter the filename:"
  vector <- readVectFile "vect.txt"
  printLn vector
