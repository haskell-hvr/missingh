module MissingH.Wash.Mail.HeaderField where

import MissingH.Wash.Utility.RFC2047

-- 
newtype Header = Header (String, String)
newtype KV = KV (String, String)
newtype MediaType = MediaType (String, String)
-- 

instance Show Header where
  show (Header (key, value)) = 
    if null value then "" else
    key ++ ':' : ' ' : encodeValue value ++ "\r\n"

instance Show KV where
  show (KV (key, value)) =
    key ++ '=' : value

instance Show MediaType where
  show (MediaType (ty, subty)) =
    ty ++ '/' : subty

-- 

mimeHeader = 
  Header ("MIME-Version", "1.0")

identHeader =
  Header ("X-Mailer", "WASH/Mail 0.1")

makeContentType mtype subtype parameters = 
  Header ("Content-Type", mtype ++ "/" ++ subtype ++ p parameters)
  where p = concat . map p1
	p1 parameter = ';' : show parameter

makeContentTransferEncoding enc =
  Header ("Content-Transfer-Encoding", enc)

makeContentDisposition name =
  Header ("Content-Disposition", name)

makeX what recipients = 
  Header (what, l recipients)
  where l [] = []
	l [xs] = xs
	l (xs:xss) = xs ++ ", " ++ l xss

makeTO = makeX "To"
makeCC = makeX "Cc"
makeBCC = makeX "Bcc"
makeSubject s = makeX "Subject" [s]

