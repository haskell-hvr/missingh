-- © 2001, 2002 Peter Thiemann
module MissingH.Wash.Mail.MIME where
-- RFC 2045
-- RFC 2046

import IO
import Random
import Char

import qualified MissingH.Wash.Utility.Base64 as Base64
import qualified MissingH.Wash.Utility.QuotedPrintable as QuotedPrintable
import MissingH.Wash.Mail.HeaderField
import qualified MissingH.Wash.Utility.RFC2279 as RFC2279
-- UTF-8


-- --------------------------------------------------------------------

textDOC subty docLines =
  DOC {	mediatype= "text",
	subtype= subty,
	textLines= docLines,
	parameters= [],
	filename= "",
	messageData="",
	parts=[]
      }

binaryDOC ty subty bindata =
  DOC {	mediatype= ty,
	subtype= subty,
	messageData= bindata,
	textLines= [],
	parameters= [],
	filename= "",
	parts=[]
      }
  
multipartDOC subty subdocs =
  DOC {	mediatype= "multipart",
	subtype= subty,
	messageData= "",
	textLines= [],
	parameters= [],
	filename= "",
	parts= subdocs
      }

data DOC =
     DOC {
	mediatype :: String,			    -- type
	subtype :: String,			    -- subtype
	parameters  :: [KV],			    -- parameters
	filename :: String,			    -- suggested filename
	-- depending on mediatype only one of the following is relevant:
	messageData :: String,			    -- data
	textLines :: [String],			    -- lines
	parts :: [DOC]				    -- data
	}

recommend_cte h doc = 
  case mediatype doc of
    "text" -> 
       case sendMode h of
         SevenBit -> "quoted-printable"
	 EightBit -> "8bit"
    "multipart" -> "7bit"
    _ ->
      case sendMode h of 
	SevenBit -> "base64"
	EightBit -> "8bit"

inventBoundary =
  inventKey 10 (init Base64.alphabet_list)
  where
    inventKey len chars =
      do g <- getStdGen
	 let candidate = take len $ map (chars !!) $ randomRs (0, length chars - 1) g
	 return ("=_" ++ candidate ++ "=_")
	 -- see RFC 2045, 6.7 for reasoning about this choice of boundary string

data SendMode =
  EightBit | SevenBit
data SendControl = 
  SendControl {
    sendH :: Handle,
    sendMode :: SendMode
  }

smtpSendControl = 
  SendControl { sendH = stdout, sendMode = SevenBit }

httpSendControl =
  SendControl { sendH = stdout, sendMode = EightBit }

instance Send DOC where
  hSend h doc =
    let cte = recommend_cte h doc in
    do boundary <- inventBoundary
       let extraParameter 
	     | mediatype doc == "multipart"  = [KV ("boundary", '\"':boundary++"\"")]
	     | mediatype doc == "text" = [KV ("charset", "utf-8")]
	     | otherwise = []
       hSend h (makeContentType (mediatype doc)
       			     (subtype doc) 
       			     (extraParameter ++ parameters doc))
       hSend h (makeContentTransferEncoding cte)
       hSend h (makeContentDisposition (filename doc))
       hSend h CRLF
       case mediatype doc of 
	 "text" -> hSendText h doc
	 "multipart" -> hSendMultipart h boundary doc
	 _ -> hSendBinary h doc

hSendText h doc =
  case sendMode h of
    EightBit -> 
      hPutStr hdl str
    SevenBit ->
      hPutStr hdl (QuotedPrintable.encode str)
  where hdl = sendH h
	str = RFC2279.encode $ flat (textLines doc)
	flat [] = []
	flat (xs:xss) = xs ++ "\r\n" ++ flat xss

hSendBinary h doc =
  case sendMode h of
    SevenBit ->
      hPutStr (sendH h) (Base64.encode (messageData doc))
    EightBit ->
      hPutStr (sendH h) (messageData doc)

hSendMultipart h boundary doc =
  do -- may send a preamble for non-MIME-able MUAs at this point
     sendParts (parts doc)
  where hdl = sendH h
	sendParts [] = 
	  do hPutStr hdl "--"
	     hPutStr hdl boundary
	     hPutStr hdl "--"
	     hSend h CRLF
	sendParts (doc:docs) =
	  do hPutStr hdl "--"
	     hPutStr hdl boundary
	     hSend h CRLF
	     hSend h doc
	     sendParts docs

data CRLF = CRLF

instance Send CRLF where
  hSend h CRLF = hPutStr (sendH h) "\n"

data Mail =
     Mail {
	to :: [String],
	subject :: String,
	cc :: [String],
	bcc :: [String],
	headers :: [Header],
	contents :: DOC
        }

simpleMail recipients subj doc =
  Mail { to= recipients, subject= subj, cc=[], bcc=[], headers=[], contents=doc }

class Send m where
  send :: m -> IO ()
  hSend :: SendControl -> m -> IO ()
  send = hSend smtpSendControl

instance Send Header where
  hSend h header = hPutStr (sendH h) (show header)

instance Send Mail where
  hSend h mail =
    do hSend h (makeTO (to mail))
       hSend h (makeSubject (subject mail))
       hSend h (makeCC (cc mail))
       hSend h (makeBCC (bcc mail))
       hSend h mimeHeader
       hSend h identHeader
       sequence (map (hSend h) (headers mail))
       hSend h (contents mail)

