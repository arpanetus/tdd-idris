import DataStore

testStore : DataStore (SString .+. SInt)
testStore = addToStore ("First", 1) $ 
            addToStore ("Second", 2) $ 
            empty

listItems : DataStore schema -> List (SchemaType schema)
listItems input with (storeView input)
    listItems empty | SNil = []
    listItems (addToStore value store) | (SAdd rec) 
        = value :: listItems store | rec

filterKeys : (test : SchemaType val_schema -> Bool) -> DataStore (SString .+. val_schema) -> List String
filterKeys test input with (storeView input)
    filterKeys test empty | SNil = []
    filterKeys test (addToStore (key, value) store) | (SAdd rec)
        = if test value 
            then key :: filterKeys test store | rec
            else filterKeys test store | rec


getValues : DataStore (SString .+. val_schema) -> List (SchemaType val_schema)
getValues  input with (storeView input)
    getValues empty | SNil = []
    getValues (addToStore (key, value) store) | (SAdd rec)
        = value :: getValues store | rec