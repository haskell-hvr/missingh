-- © 2003 Peter Thiemann
{-|
  Implements UTF-8 encoding

  UCS-4 range (hex.)           UTF-8 octet sequence (binary)
  0000 0000-0000 007F   0xxxxxxx
  0000 0080-0000 07FF   110xxxxx 10xxxxxx
  0000 0800-0000 FFFF   1110xxxx 10xxxxxx 10xxxxxx
  0001 0000-001F FFFF   11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
  0020 0000-03FF FFFF   111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
  0400 0000-7FFF FFFF   1111110x 10xxxxxx ... 10xxxxxx
-}
module MissingH.Wash.Utility.RFC2279 (encode, decode) where

import Char


-- |maps Unicode string to list of octets
encode :: String -> String

-- |maps list of octets in UTF-8 encoding to Unicode string
decode :: String -> String

factors = iterate (* 0x40) 1
f1 = factors !! 1
f2 = factors !! 2
f3 = factors !! 3
f4 = factors !! 4
f5 = factors !! 5

encode [] = []
encode (x:xs) = 
  let r0 = ord x in
  if r0 < 0x80 then
    x : encode xs
  else if r0 < 0x800 then
    let c1 = 0xC0 + r0 `div` f1
	c2 = 0x80 + r0 `mod` f1
    in  chr c1 : chr c2 : encode xs
  else if r0 < 0x10000 then
    let c1 = 0xE0 + r0 `div` f2
	r1 = r0 `mod` f2
	c2 = 0x80 + r1 `div` f1
	c3 = 0x80 + r1 `mod` f1
    in  chr c1 : chr c2 : chr c3 : encode xs
  else if r0 < 0x200000 then
    let c1 = 0xF0 + r0 `div` f3
	r1 = r0 `mod` f3
	c2 = 0x80 + r1 `div` f2
	r2 = r1 `mod` f2
	c3 = 0x80 + r2 `div` f1
	c4 = 0x80 + r2 `mod` f1
    in  chr c1 : chr c2 : chr c3 : chr c4 : encode xs
  else if r0 < 0x4000000 then
    let c1 = 0xF8 + r0 `div` f4
	r1 = r0 `mod` f4
	c2 = 0x80 + r1 `div` f3
	r2 = r1 `mod` f3
	c3 = 0x80 + r2 `div` f2
	r3 = r2 `mod` f2
	c4 = 0x80 + r3 `div` f1
	c5 = 0x80 + r3 `mod` f1
    in  chr c1 : chr c2 : chr c3 : chr c4 : chr c5 : encode xs
  else 
    let c1 = 0xFC + r0 `div` f5
	r1 = r0 `mod` f5
	c2 = 0x80 + r1 `div` f4
	r2 = r1 `mod` f4
	c3 = 0x80 + r2 `div` f3
	r3 = r2 `mod` f3
	c4 = 0x80 + r3 `div` f2
	r4 = r3 `mod` f2
	c5 = 0x80 + r4 `div` f1
	c6 = 0x80 + r4 `mod` f1
    in  chr c1 : chr c2 : chr c3 : chr c4 : chr c5 : chr c6 : encode xs


decode [] = []
decode (x : xs) =
  let ordx = ord x in 
  if ordx < 0x80 then
    x : decode xs
  else if ordx < 0xC0 then
    error "UTF-8 decoding out of sync"
  else if ordx < 0xE0 then
    decoden 1 (ordx - 0xC0) xs
  else if ordx < 0xF0 then
    decoden 2 (ordx - 0xE0) xs
  else if ordx < 0xF8 then
    decoden 3 (ordx - 0xF0) xs
  else if ordx < 0xFC then
    decoden 4 (ordx - 0xF8) xs
  else if ordx < 0xFE then
    decoden 5 (ordx - 0xFC) xs
  else
    error "UTF-8 decoding found illegal start octet"
      
decoden :: Int -> Int -> String -> String
decoden 0 v xs =
  chr v : decode xs
decoden n v (x : xs) =
  let ordx = ord x
      v' = f1 * v + ordx - 0x80
  in 
  if ordx >= 0x80 && ordx < 0xC0 then
    decoden (n-1) v' xs
  else 
    error "UTF-8 decoding found illegal continuation octet"

	       
