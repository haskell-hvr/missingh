module MissingH.Wash.Utility.QuotedPrintable 
       (encode, encode', decode
       -- deprecated: encode_quoted, encode_quoted', decode_quoted
       ) where

import Char
import MissingH.Wash.Utility.Hex

encode, encode', decode :: String -> String
encode = encode_quoted
encode' = encode_quoted'
decode = decode_quoted


encode_hexadecimal c = '=' : showHex2 c

quoted_printable x = 
  ox >= 33 && ox <= 126 && ox /= 61
  where ox = ord x

end_of_line [] = True
end_of_line ('\r':'\n':_) = True
end_of_line _ = False

encode_quoted' (x:xs) | x `elem` "\t " = 
  if end_of_line xs then encode_hexadecimal (ord x) ++ encode_quoted' xs
                    else x : encode_quoted' xs
encode_quoted' (x:xs) | quoted_printable x = x : encode_quoted' xs
encode_quoted' ('\r':'\n':xs) = '\r':'\n': encode_quoted' xs
encode_quoted' (x:xs) = encode_hexadecimal (ord x) ++ encode_quoted' xs
encode_quoted' [] = ""

encode_quoted = softLineBreak 76 . encode_quoted'

softLineBreak n [] = "\r\n"
softLineBreak 0 xs | not (end_of_line xs) = '=':'\r':'\n': softLineBreak 76 xs
softLineBreak n ('\r':'\n':xs) = '\r':'\n': softLineBreak 76 xs
softLineBreak n (xs@('=':_)) | n < 4 = '=':'\r':'\n': softLineBreak 76 xs
softLineBreak n (x:xs) = x : softLineBreak (n-1) xs

decode_quoted [] = []
decode_quoted ('=':'\r':'\n':xs) =
  decode_quoted xs
decode_quoted ('=':'\n':xs) =
  decode_quoted xs
decode_quoted ('=':upper:lower:xs) | isHexdigit upper && isHexdigit lower = 
  chr (16 * hexDigitVal upper + hexDigitVal lower) : decode_quoted xs
decode_quoted (x:xs) = 
  x : decode_quoted xs
