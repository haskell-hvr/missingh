-- © 2002 Peter Thiemann
-- |Implements RFC 2045 MIME coding.
module MissingH.Wash.Utility.Base64
       (encode, encode', decode, decode'
       ,alphabet_list
       )
       where

import Array
import Char

--
-- |Yields encoded input cropped to lines of less than 76 characters. Directly
-- usable as email body.
encode :: String -> String
encode = encode_base64
-- |yields continuous stream of bytes.
encode' :: String -> String
encode' = encode_base64'
-- |Directly applicable to email body.
decode :: String -> String
decode = decode_base64
-- |Only applicable to stream of Base64 characters.
decode' :: String -> String
decode' = decode_base64'
-- |Applicable to list of lines.
decode_lines :: [String] -> String
decode_lines = decode_base64_lines

-- --------------------------------------------------------------------
-- |Base64 alphabet in encoding order.
alphabet_list :: String
alphabet_list =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

encode_base64_alphabet_index =
  zip [0 .. (63::Int)] alphabet_list

decode_base64_alphabet_index =
  zip alphabet_list [0 .. (63::Int)]

encode_base64_alphabet =
  array (0 :: Int, 63 :: Int) encode_base64_alphabet_index

decode_base64_alphabet =
  array (' ','z') decode_base64_alphabet_index

base64_character =
  array (chr 0, chr 255) [(c, c `elem` alphabet_list || c == '=') | c <- [chr 0 .. chr 255]]

encode_base64 = linebreak 76 . encode_base64'

linebreak m xs = lb m xs
  where
    lb n [] = "\r\n"
    lb 0 xs = '\r':'\n': lb m xs
    lb n (x:xs) = x: lb (n-1) xs

encode_base64' [] = []

encode_base64' [ch] = 
  encode_base64_alphabet!b1 : 
  encode_base64_alphabet!b2 :
  "=="
  where (b1, b2, _, _) = encode_base64_group (ch, chr 0, chr 0)

encode_base64' [ch1, ch2] =
  encode_base64_alphabet!b1 : 
  encode_base64_alphabet!b2 :
  encode_base64_alphabet!b3 :
  "="
  where (b1, b2, b3, _) = encode_base64_group (ch1, ch2, chr 0)

encode_base64' (ch1: ch2: ch3: rest) =
  encode_base64_alphabet!b1 : 
  encode_base64_alphabet!b2 :
  encode_base64_alphabet!b3 :
  encode_base64_alphabet!b4 :
  encode_base64' rest
  where (b1, b2, b3, b4) = encode_base64_group (ch1, ch2, ch3)

-- 111111 112222 222233 333333
encode_base64_group (ch1, ch2, ch3) = (b1, b2, b3, b4)
  where o1 = ord ch1
	o2 = ord ch2
	o3 = ord ch3
	b1 = o1 `div` 4
	b2 = (o1 `mod` 4) * 16 + o2 `div` 16
	b3 = (o2 `mod` 16) * 4 + o3 `div` 64
	b4 = o3 `mod` 64

decode_base64_group (b1, b2, b3, b4) = (ch1, ch2, ch3)
  where ch1 = chr (b1 * 4 + b2 `div` 16)
	ch2 = chr (b2 `mod` 16 * 16 + b3 `div` 4)
	ch3 = chr (b3 `mod` 4 * 64 + b4)

decode_base64' [] = []

decode_base64' [cin1, cin2, '=', '='] = [cout1]
  where (cout1, _, _) = 
          decode_base64_group (decode_base64_alphabet!cin1
	  		      ,decode_base64_alphabet!cin2
			      ,0
			      ,0)

decode_base64' [cin1, cin2, cin3, '='] = [cout1, cout2]
  where (cout1, cout2, _) = 
          decode_base64_group (decode_base64_alphabet!cin1
	  		      ,decode_base64_alphabet!cin2
			      ,decode_base64_alphabet!cin3
			      ,0)

decode_base64' (cin1: cin2: cin3: cin4: rest) = 
  cout1: cout2: cout3: decode_base64' rest
  where (cout1, cout2, cout3) = 
          decode_base64_group (decode_base64_alphabet!cin1
	  		      ,decode_base64_alphabet!cin2
			      ,decode_base64_alphabet!cin3
			      ,decode_base64_alphabet!cin4)

decode_base64 = decode_base64' . filter (base64_character!)

decode_base64_lines = decode_base64' . concat

