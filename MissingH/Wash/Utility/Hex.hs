-- © 2001, 2003 Peter Thiemann
module MissingH.Wash.Utility.Hex where

import Array
import Char

hexdigit :: Int -> Char
hexdigit i = hexdigits ! i

hexdigits' = "0123456789ABCDEF"
alternative_digits = "abcdef"
alternative_indices :: [(Int, Char)]
alternative_indices = zip [10..15] alternative_digits
hexdigits'_indices :: [(Int, Char)]
hexdigits'_indices = [(i, hexdigits'!!i) | i <- [0..15]]

hexdigits = array (0, 15) hexdigits'_indices

fromHexdigits =
  array (chr 0, chr 127) 
        (map (\ (x,y) -> (y, x)) (hexdigits'_indices ++ alternative_indices))

isHexdigitArray =
  array (chr 0, chr 127)
	(map (\ c -> (c, isHexdigit c)) [chr 0 .. chr 127])
  where
    isHexdigit :: Char -> Bool
    isHexdigit x = 
      (x >= '0' && x <= '9') || 
      (x >= 'a' && x <= 'f') ||
      (x >= 'A' && x <= 'F')

isHexdigit :: Char -> Bool
isHexdigit x = 
  x <= chr 127 && isHexdigitArray ! x

showHex2 :: Int -> String
showHex2 ox = showsHex 2 ox ""

showsHex :: Int -> Int -> ShowS
showsHex 0 x = id
showsHex i x = let (d,m) = x `divMod` 16 in showsHex (i-1) d . showChar (hexdigits ! m)

hexDigitVal :: Char -> Int
hexDigitVal x | isHexdigit x = fromHexdigits ! x
	      | otherwise    = 0

allDigits = hexdigits' ++ alternative_digits

