-- © 2003 Peter Thiemann
module MissingH.Wash.Utility.JavaScript where

import Char

import MissingH.Wash.Utility.Hex

jsShow :: String -> String
jsShow xs = '\'' : g xs
  where
    g "" = "'"
    g (x:xs) = 
      case x of
	'\'' -> h x xs
	'\"' -> h x xs
	'<' -> h x xs
	'>' -> h x xs
	'&' -> h x xs
	x | isPrint x -> x : g xs
	  | otherwise -> h x xs
    h x xs =
      let ox = ord x in
      if ox < 256 then
	 '\\' : 'x' : showsHex 2 ox (g xs)
      else
	 '\\' : 'u' : showsHex 4 ox (g xs)
				 
