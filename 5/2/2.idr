import System

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
    then pure (Just (cast input))
    else pure Nothing


guess : (target : Nat) -> IO ()
guess target = do
    x <- readNumber
    case x of
      Nothing => do
        putStrLn "Wrong, try again!\n"
        guess target
      Just x => if x == target
        then putStrLn ("Good! You guessed " ++ (show target))
        else do
          putStrLn "Wrong, try again!\n"
          guess target

main : IO ()
main = do
  secs <- System.time
  putStrLn "Guess the number!"
  guess (cast (mod secs 100))
