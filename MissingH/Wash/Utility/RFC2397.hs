module MissingH.Wash.Utility.RFC2397 where

import MissingH.Wash.Utility.URLCoding
import MissingH.Wash.Utility.Base64

import qualified MissingH.Wash.Utility.Base64 as Base64
import qualified MissingH.Wash.Utility.URLCoding as URLCoding

data ENC = BASE64 | URL
  deriving Eq

-- |maps (mediatype, contents) to data URL
encode :: (String, String) -> String
encode (mediatype, thedata) =
  "data:" ++ mediatype ++ ";base64," ++ Base64.encode' thedata

-- |maps data URL to @Just (mediatype, contents)@ or @Nothing@ in case of a
-- syntax error.
decode :: String -> Maybe (String, String)
decode url = 
  let (scheme, rest) = break (==':') url in
  case rest of
    ':' : contents | scheme == "data" -> 
      decodeContents contents
    _ -> Nothing

decodeContents xs =
  let (prefix, restdata) = break (==',') xs in
  case restdata of
    ',' : thedata ->
      decodePrefix prefix thedata
    _ -> Nothing
      
decodePrefix prefix thedata =
  let fragments = breakList (==';') prefix 
      enc = case reverse fragments of
	      ("base64":_) -> BASE64
	      _ -> URL
      mediapart | enc == BASE64 = init fragments
                | otherwise     = fragments
  in
  case mediapart of
    (xs:_) ->
      case break (=='/') xs of
	(_, []) -> 
	  decodeData ("text/plain" : mediapart) enc thedata
	_ ->
	  decodeData mediapart enc thedata
    _ ->  decodeData ["text/plain", "charset=US-ASCII"] enc thedata

decodeData mediatype enc thedata =
  Just ( unparse mediatype
       , case enc of
	   URL    -> URLCoding.decode thedata
	   BASE64 -> Base64.decode thedata
       )

breakList :: (x -> Bool) -> [x] -> [[x]]
breakList p xs =
  let (pre, post) = break p xs in
  case post of
    [] -> [pre]
    y:ys -> pre : breakList p ys

unparse [] = ""
unparse [xs] = xs
unparse (xs:xss) = xs ++ ';' : unparse xss
