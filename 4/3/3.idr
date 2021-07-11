module Main

import Data.Vect

data DataStore : Type where
  MkData : (size: Nat) ->
           (items: Vect size String) ->
           DataStore

size : DataStore -> Nat
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'


addToStore : DataStore -> String -> DataStore
addToStore (MkData size items') newItem = MkData _ (addToData items')
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newItem]
    addToData (item :: items') = item :: addToData items'


data Command = Add String
             | Search String
             | Get Integer
             | Size
             | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "search" str = Just (Search str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "size" "" = Just Size
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)


getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store
  = let storeItems = items store in
      case integerToFin pos (size store) of
         Nothing => Just ("Out of range\n", store)
         Just id => Just (index id storeItems ++ "\n", store)


genVect : {n: Nat} -> Integer -> Vect n String -> Vect n Integer
genVect {n = Z} num [] = []
genVect {n = (S k)} num (x :: xs) = num :: genVect (num+1) xs


search : (str: String) -> (store: DataStore) -> String
search str store
  = let storeItems = items store in
      compileMatches $ Pairs.DPair.snd $ filter ((isInfixOf str) . fst) (zip storeItems (genVect 0 storeItems))
        where
          compileMatches : Vect len (String, Integer) -> String
          compileMatches [] = ""
          compileMatches ((str, idx)::xs) = "\n\t" ++ (show idx) ++ ". "++ str ++ compileMatches xs


processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = case parse inp of
                              Nothing => Just ("Invalid command\n", store)
                              Just (Add item) =>
                                Just ("ID: " ++ show (size store) ++ "\n", addToStore store item)
                              Just (Search str) =>
                                Just ("Search result:" ++ search str store  ++ "\n", store)
                              Just (Get pos) => getEntry pos store
                              Just Size => Just ("Size: " ++ show (size store) ++ "\n", store)
                              Just Quit => Nothing


main : IO()
main = replWith (MkData _ []) "Command: " processInput
