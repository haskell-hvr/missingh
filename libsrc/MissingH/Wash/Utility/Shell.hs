-- © 2002 Peter Thiemann
-- |Defines functions for shell quotation.
module MissingH.Wash.Utility.Shell where

import Char

-- |Shell meta characters are /! & ; \` \' \" | * ? ~ \< \> ^ ( ) [ ] true $ n r/
metaCharacters :: String
metaCharacters = " !&;`\'\"|*?~<>^()[]$\\%{}"

-- |Quotes all shell meta characters and removes non printable ones.
quote :: String -> String
quote "" = ""
quote (x:xs) | isPrint x =
	       if x `elem` metaCharacters 
	       then '\\' : x : quote xs
	       else x : quote xs
	     | otherwise = 
	       quote xs

