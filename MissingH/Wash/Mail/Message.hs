module MissingH.Wash.Mail.Message where

import MissingH.Wash.Mail.HeaderField

data Message =
     Singlepart 
     	{ getHeaders 		:: [Header]
	, getLines   		:: [String]
	, getDecoded		:: [Char]
	, getContentType 	:: ContentType
	, getContentTransferEncoding :: ContentTransferEncoding
	, getContentDisposition :: ContentDisposition
	}
   | Multipart
	{ getHeaders		:: [Header]
	, getLines		:: [String]
	, getParts		:: [Message]
	, getContentType 	:: ContentType
	, getContentTransferEncoding :: ContentTransferEncoding
	, getContentDisposition :: ContentDisposition
	}
    deriving Show

isSinglePart :: Message -> Bool
isSinglePart (Singlepart {}) = True
isSinglePart _ = False

isMultiPart :: Message -> Bool
isMultiPart (Multipart {}) = True
isMultiPart _ = False

showHeader :: Header -> String
showHeader (Header (n, v)) = n ++ ": " ++ v

showParameters :: [(String, String)] -> String -> String
showParameters c_parameters =
    foldr (\(n,v) f -> showString " ;" .
    		       showString n .
		       showString "=\"" .
		       showString v .
		       showChar '\"' . f) id c_parameters

data ContentType = 
	ContentType String -- type
		    String -- subtype
		    [(String, String)] -- parameters
instance Show ContentType where
  showsPrec i (ContentType c_type c_subtype c_parameters) =
    showString "Content-Type: " .
    showString c_type .
    showChar '/' .
    showString c_subtype .
    showParameters c_parameters
    
data ContentTransferEncoding =
	ContentTransferEncoding String
instance Show ContentTransferEncoding where
  showsPrec i (ContentTransferEncoding cte) =
    showString "Content-Transfer-Encoding: " .
    showString cte

data ContentDisposition =
	ContentDisposition String [(String, String)]
instance Show ContentDisposition where
  showsPrec i (ContentDisposition cdn c_parameters) =
    showString "Content-Disposition: " .
    showString cdn .
    showParameters c_parameters

data ContentID =
	ContentID String
instance Show ContentID where
  showsPrec i (ContentID cid) =
    showString "Content-ID: " .
    showString cid

data ContentDescription =
	ContentDescription String
instance Show ContentDescription where
  showsPrec i (ContentDescription txt) =
    showString "Content-Description: " .
    showString txt
