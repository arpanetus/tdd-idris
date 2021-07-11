import System

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
    then pure (Just (cast input))
    else pure Nothing


guess : (target : Nat) -> (guesses: Nat) -> IO ()
guess target guesses = do
    putStrLn ("So far " ++ (show guesses) ++ " guesses :D")
    x <- readNumber
    case x of
      Nothing => do
        putStrLn "Wrong, try again!\n"
        guess target (guesses+1)
      Just x => if x == target
        then putStrLn ("Good! You guessed " ++ (show target))
        else do
          putStrLn "Wrong, try again!\n"
          guess target (guesses+1)

main : IO ()
main = do
  secs <- System.time
  putStrLn "Guess the number!"
  guess (cast (mod secs 100)) 0
