-- © 2001, 2002 Peter Thiemann
-- |Implements coding of non-alphanumeric characters in URLs and CGI-requests.
module MissingH.Wash.Utility.URLCoding (encode, decode) where

import Char
import MissingH.Wash.Utility.Hex

encode, decode :: String -> String
encode = urlEncode
decode = urlDecode

urlEncode :: String -> String
urlEncode "" = ""
urlEncode (x:xs) | isAlphaNum x = x : urlEncode xs
		 | x == ' '     = '+' : urlEncode xs
		 | otherwise    = '%' : showHex2 (ord x) ++ urlEncode xs

urlDecode :: String -> String
urlDecode "" = ""
urlDecode ('+':xs) =
	' ' : urlDecode xs
urlDecode ('%':upper:lower:xs) =
	chr (16 * hexDigitVal upper + hexDigitVal lower) : urlDecode xs
urlDecode (x:xs) = 
	x : urlDecode xs

