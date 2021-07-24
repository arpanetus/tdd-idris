import Data.Vect

infixr 5 .+.

data Schema = SString
            | SInt
            | SChar
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType SChar = Char
SchemaType (x .+. y) = (SchemaType x, SchemaType y)


data Command: Schema -> Type where
    SetSchema : (newschema : Schema) -> Command schema
    Add: SchemaType schema' -> Command schema'
    Get : Integer -> Command schema'
    GetAll : Command schema'
    Size : Command schema'
    Quit : Command schema'
--  Search : String -> Command schema'

parsePrefix : (schema' : Schema) -> (input : String) -> Maybe (SchemaType schema', String)
parsePrefix SInt input = do 
            (num, rest) <- Just (span isDigit input)
            Just (cast num, ltrim rest)

parsePrefix SChar input = getCharQuoted (unpack input)
  where 
    getCharQuoted : List Char -> Maybe(Char, String)
    getCharQuoted (symbol :: rest) =
      Just (symbol, ltrim (pack rest))
    getCharQuoted _ = Nothing

parsePrefix SString input = getQuoted (unpack input)
  where
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: xs) = do 
      (quoted, '"' :: rest) <- Just (span (/= '"') xs) 
      Just (pack quoted, ltrim (pack rest))
      
    getQuoted _ = Nothing

parsePrefix (schemal .+. schemar) input = do 
  (l_val, input') <- parsePrefix schemal input
  (r_val, input'') <- parsePrefix schemar input'
  Just ((l_val, r_val), input'')

parseBySchema : (schema' : Schema) -> (rest : String) -> Maybe (SchemaType schema')
parseBySchema schema' input = do 
  (res, "") <- parsePrefix schema' input
  Just res

parseSchema : List String -> Maybe Schema
parseSchema ("String" :: xs) = 
  case xs of 
    [] => Just SString
    _ => do xs_sch <- parseSchema xs  
            Just (SString .+. xs_sch)

parseSchema ("Int" :: xs) = 
  case xs of 
    [] => Just SInt 
    _ => do xs_sch <- parseSchema xs  
            Just (SInt .+. xs_sch)

parseSchema ("Char" :: xs) = 
  case xs of 
    [] => Just SChar 
    _ => do xs_sch <- parseSchema xs  
            Just (SChar .+. xs_sch)

            
parseSchema _ = Nothing
  

parseCommand : (schema' : Schema) -> (cmd : String) -> (args : String) -> Maybe (Command schema')

parseCommand schema' "add" rest = do 
  restok <- parseBySchema schema' rest
  Just (Add restok)

-- parseCommand "search" str = Just (Search str)
parseCommand schema' "get" ""  = Just GetAll
parseCommand schema' "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand schema' "size" "" = Just Size
parseCommand schema' "quit" "" = Just Quit
parseCommand schema "schema" rest = 
  case parseSchema (words rest) of 
    Nothing => Nothing
    Just schemaok => Just (SetSchema schemaok)

parseCommand _ _ _ = Nothing

parse : (schema' : Schema) -> (input : String) -> Maybe (Command schema')
parse schema' input = case span (/= ' ') input of
                   (cmd, args) => parseCommand schema' cmd (ltrim args)


record DataStore where
  constructor MkData
  schema' : Schema
  size' : Nat
  items' : Vect size' (SchemaType schema')

size : DataStore -> Nat
size (MkData schema' size' items') = size'

schema : DataStore -> Schema
schema (MkData schema' size' items') = schema'

items : (store : DataStore) -> Vect (size store) (SchemaType (schema store))
items (MkData schema' size' items') = items'


addToStore : (store: DataStore) -> SchemaType (schema' store) -> DataStore
addToStore (MkData schema' size store) newitem = MkData schema' _ (addToData store)
  where
    addToData : Vect oldsize (SchemaType schema') ->
                Vect (S oldsize) (SchemaType schema')
    addToData [] = [newitem]
    addToData (item :: items) = item :: addToData items


display :  SchemaType schema' -> String
display {schema' = SString} item = show item
display {schema' = SInt} item = show item
display {schema' = SChar} item = show item
display {schema' = (x .+. y)} (iteml, itemr) = display iteml ++ ", " ++ display itemr


getEntry : (pos : Integer) -> (store : DataStore) ->
           Maybe (String, DataStore)
getEntry pos store = do 
  Just id <- Just(integerToFin pos (size store)) | Nothing => Just ("Out of range\n", store)
  Just (display (index id (items store)) ++ "\n", store)


genVect : {n: Nat} -> Integer -> Vect n any -> Vect n Integer
genVect {n = Z} num [] = []
genVect {n = (S k)} num (x :: xs) = num :: genVect (num+1) xs
                        

getEntries : (store : DataStore) -> String
getEntries store = conCatEntries $ zip (items store) (genVect 0 (items store)) where
  conCatEntries : Vect len ((SchemaType schema'), Integer) -> String
  conCatEntries [] = ""
  conCatEntries ((schemaItem, number) :: items) = 
    let 
      rest = conCatEntries items
      cur = display schemaItem
      num = show number ++ ". "
      in num ++ cur ++ "\n" ++rest

setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema' = do
   Z <- Just(size store) | Nothing 
   Just (MkData schema' _ [])
  
-- I guess one could use 'do' here, but I'm not that one, since I have no idea
processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = 
  case parse (schema store) input of
    Nothing => Just ("Invalid command\n", store)
    Just (Add item) => 
      Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
    Just (SetSchema schema') => 
      case setSchema store schema' of 
        Nothing => Just ("Can't update schema\n", store)  
        Just store' => Just ("OK\n", store')
    Just (Get pos) => getEntry pos store
    Just GetAll => Just ((getEntries store), store)
    Just Quit => Nothing

main : IO ()
main = replWith (MkData SString _ [])
                "Command: " processInput
