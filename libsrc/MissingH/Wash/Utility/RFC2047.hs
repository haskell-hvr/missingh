module MissingH.Wash.Utility.RFC2047 where
-- decoding of header fields
import Char
import List

import qualified MissingH.Wash.Utility.Base64 as Base64
import qualified MissingH.Wash.Utility.QuotedPrintable as QuotedPrintable
import MissingH.Wash.Utility.Hex
import Text.ParserCombinators.Parsec

lineString =
  do initial <- many (noneOf "\n\r=")
     rest <- option "" (do xs <- try encoded_words <|> string "=" 
			   ys <- lineString
			   return (xs ++ ys))
     return (initial ++ rest)

especials = "()<>@,;:\\\"/[]?.="
tokenchar = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~" \\ especials
p_token = many1 (oneOf tokenchar)
p_encoded_text = many1 $ oneOf "!\"#$%&'()*+,-./0123456789:;<=>@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
allchar = "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\a\b\t\n\v\f\r\SO\SI\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\EM\SUB\ESC\FS\GS\RS\US !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\DEL"

-- supress linear white space between adjacent encoded_word
encoded_words =
  do ew <- encoded_word
     ws <- many space
     option (ew++ws) (encoded_words >>= \ews -> return (ew++ews))

encoded_word =
  do string "=?"
     charset <- p_token
     char '?'
     encoding <- p_token
     char '?'
     encoded_text <- p_encoded_text
     string "?="
     return $ decode charset (map toUpper encoding) encoded_text

decode charset "B" encoded_text =
  Base64.decode' encoded_text
decode charset "Q" encoded_text =
  decode_quoted encoded_text
decode charset encoding encoded_text =
  error ("Unknown encoding: " ++ encoding)
  
decode_quoted [] = []
decode_quoted ('=':upper:lower:xs) = 
  chr (16 * hexDigitVal upper + hexDigitVal lower) : decode_quoted xs
decode_quoted ('_':xs) = 
  ' ' : decode_quoted xs
decode_quoted (x:xs) = 
  x : decode_quoted xs

-- --------------------------------------------------------------------
-- RFC 2047: encoding of header fields

encodeWord w =
  "=?" ++ charset ++ "?" ++ encoding ++ "?" ++ QuotedPrintable.encode' w ++ "?="
  where encoding = "q"
	charset  = "iso-8859-1"

encodeValue v = 
  case span (not . flip elem " ()<>@.!,") v of
    ([], []) -> []
    (word, []) -> maybeEncode word
    (word, x:rest) -> maybeEncode word ++ x : encodeValue rest

maybeEncode word | all p word = word
                 | otherwise = encodeWord word
  where p x = let ox = ord x in ox >= 33 && ox <= 126
