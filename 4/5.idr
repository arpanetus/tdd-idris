module Main

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a

maxMaybe (Just a) (Just b) = case a>b of
                              True => Just a
                              False => Just b
maxMaybe (Just a) Nothing = Just a
maxMaybe Nothing (Just b) = Just b
maxMaybe Nothing Nothing = Nothing


main : IO()
main = do
  putStrLn (show (maxMaybe (Just 4) (Just 5)))
  putStrLn (show (maxMaybe (Just 4) Nothing))
