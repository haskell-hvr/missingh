-- Base32 standard:
-- http://www.ietf.org/rfc/rfc3548.txt
-- Author: Niklas Deutschmann

module MissingH.Wash.Utility.Base32 (encode, decode) where

import Bits
import Char
import List

encode :: String -> String
encode = encBase32

decode :: String -> String
decode = decBase32

-- Partitions a list into groups of length n.
makeGroups :: Int -> [a] -> [[a]]
makeGroups 0 lst = error "makeGroups: Invalid group length"
makeGroups n [] = []
makeGroups n lst =  take n lst : makeGroups n (drop n lst)

-- Converts an array of characters to a large number, generalized for
-- characters of any number of bits and any alphabet.
-- charLength: Number of bits in a character
-- ordFunc:	Function that is used for mapping characters to numbers.
makeBits charLength ordFunc str = foldr (+) 0 bitValues
	where 
	bitValues = zipWith (\a b -> intVal a `shiftL` b) (reverse str) [0,charLength..]
	intVal a = toInteger (ordFunc a)
	
makeBitsASCII = makeBits 8 ord
makeBitsBase32 = makeBits 5 b32Ord

-- Converts an array of characters into a m-bit number, where m is
-- is smallest multiple of n that is greater or equal to the 
-- (length of str) * chrSize.
-- Extension of "makeBits".
makeMultipleOfNBits bitFunc charSize n str 
	| len `mod` n == 0	= bitFunc str
	| otherwise			= (bitFunc str) `shiftL` (remBitCount len)
	where 
	remBitCount m = (0 - (m `mod` n) + n) `mod` n
	len = length str * charSize;

makeMultipleOfNBitsASCII = makeMultipleOfNBits (makeBits 8 ord) 8 
makeMultipleOfNBitsBase32 = makeMultipleOfNBits (makeBits 5 b32Ord) 5

-- The Base32 alphabet
-- Int -> Base32 character
b32Chr n = b32tab !! (fromEnum n)
	where
	b32tab = ['A'..'Z'] ++ ['2'..'7']

-- Base32 character -> Int
b32Ord c 
	| c >= 'A' && c <= 'Z'	= ord(c) - 65
	| c >= '2' && c <= '7'	= ord(c) - 24
	| otherwise = error "b32Ord: No Base character"
	
-- Encodes one block (1-5 ASCII Characters)
encBase32Block str 
	| len == 0	= ""
	| len == 1	= concat (b32Map [5,0]) ++ "======"
	| len == 2	= concat (b32Map [15,10..0]) ++ "===="
	| len == 3	= concat (b32Map [20,15..0]) ++ "==="
	| len == 4	= concat (b32Map [30,25..0]) ++ "="
	| len == 5	= concat (b32Map [35,30..0])
	| otherwise = error "encBase32Block: Invalid block length"
 	where 
	b32Map = map (\x -> [b32Chr(bitStr `shiftR` x .&. 31)])
	bitStr = makeMultipleOfNBitsASCII 5 str
	len = length str

-- Decodes one block (2,4,5,7 or 8 Base32 character + '=' padding character)
decBase32Block str
	| len == 0 = ""
	| len == 2 = concat . (shiftAndMap [0] 2) . makeBitsBase32 $ code
	| len == 4 = concat . (shiftAndMap [8,0] 4) . makeBitsBase32 $ code
	| len == 5 = concat . (shiftAndMap [16,8,0] 1) . makeBitsBase32 $ code
	| len == 7 = concat . (shiftAndMap [24,16..0] 3) . makeBitsBase32 $ code
	| len == 8 = concat . (shiftAndMap [32,24..0] 0) . makeBitsBase32 $ code
	| otherwise = error "decBase32Block: Invalid block length"
	where
	shiftAndMap sf n = (asciiMap sf) . (`shiftR` n)
	asciiMap sf c = map (\x -> [chr . fromEnum $ (c `shiftR` x .&. 255)]) sf
	len = length code
	code = filter (/= '=') str
		
encBase32 :: String -> String
encBase32 = concat . map encBase32Block . makeGroups 5

decBase32 :: String -> String
decBase32 = concat . map decBase32Block . makeGroups 8


