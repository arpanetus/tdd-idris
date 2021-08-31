import System

%default total

data InfIO : Type where
    Do : IO a -> (a -> Inf InfIO) -> InfIO

(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

run : Fuel -> InfIO -> IO ()
run Dry p = putStrLn "Out of fuel"
run (More fuel) (Do c f) = 
    do res <- c
       run fuel (f res)

partial
forever : Fuel
forever = More forever


totalREPL : (prompt : String) -> (action : String -> String) -> InfIO
totalREPL prompt action = do
    putStr prompt
    val <- getLine
    putStr $ action val
    totalREPL prompt action

