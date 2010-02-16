import MissingH.List
main = do
   c <- getContents
   putStr (unlines(filter (\line -> contains "Haskell" line)
                          (lines c)))

