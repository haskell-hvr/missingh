module MissingH.Wash.Mail.MailParser where

-- see RFC 2822
-- TODO: check against their definition of token
import Char
import List
import Maybe
-- 
import Text.ParserCombinators.Parsec
-- 
import qualified MissingH.Wash.Utility.Base64 as Base64
import qualified MissingH.Wash.Utility.QuotedPrintable as QuotedPrintable
import qualified MissingH.Wash.Utility.RFC2047 as RFC2047
import MissingH.Wash.Utility.RFC2047 (p_token)
import MissingH.Wash.Mail.Message
import MissingH.Wash.Mail.HeaderField

parseMessageFromFile :: FilePath -> IO (Either ParseError RawMessage)
parseMessageFromFile fname =
  parseFromFile message fname

parseMessageFromString :: String -> Either ParseError RawMessage
parseMessageFromString str =
  parse message "MailParser" str

parseDateTimeFromString :: String -> Either ParseError DateTime2822
parseDateTimeFromString str =
  parse parseDateTime "DateTimeParser" (' ':str)

data RawMessage =
     RawMessage
     	{ rawHeaders	:: [Header]
	, rawLines	:: [String]
	}
  deriving Show

lexeme p = do x <- p; many ws1; return x
literalString = do char '\"'
		   str <- many (noneOf "\"\\" <|> quoted_pair)
		   char '\"'
		   return str

no_ws_ctl_chars = map chr ([1..8] ++ [11,12] ++ [14..31] ++ [127])
no_ws_ctl = oneOf no_ws_ctl_chars

text_chars = map chr ([1..9] ++ [11,12] ++ [14..127])
p_text = oneOf text_chars

quoted_pair = do char '\\'
		 p_text

-- RFC 2045, 5.1 says: 
-- "The type, subtype, and parameter names are not case sensitive."

p_parameter =
  do lexeme $ char ';'
     p_name <- lexeme $ p_token
     lexeme $ char '='
     p_value <- literalString <|> p_token
     return (map toLower p_name, p_value)

p_contentType = 
  do many ws1
     c_type <- p_token
     lexeme $ char '/'
     c_subtype <- lexeme $ p_token
     c_parameters <- many p_parameter
     return $ ContentType (map toLower c_type) (map toLower c_subtype) c_parameters

-- RFC 2045, 6.1
-- "these values are not case sensitive"

p_contentTransferEncoding =
  do many ws1
     c_cte <- RFC2047.p_token
     return $ ContentTransferEncoding (map toLower c_cte)

p_contentDisposition =
  do many ws1
     c_cd <- RFC2047.p_token
     c_parameters <- many p_parameter
     return $ ContentDisposition (map toLower c_cd) c_parameters

p_contentID = 
  do many ws1
     c_cid <- RFC2047.p_token
     return $ ContentID c_cid

p_contentDescription =
  do many ws1
     c_desc <- many lineChar
     return $ ContentDescription c_desc

crLf = try (string "\n\r" <|> string "\r\n") <|> string "\n" <|> string "\r"

fws =
  do many1 ws1
     option "" (do crLf
		   many1 ws1)
  <|> 
  do crLf
     many1 ws1

ws1 = oneOf " \t"
lineChar = noneOf "\n\r"
headerNameChar = noneOf "\n\r:"

header = do name <- many1 headerNameChar
	    char ':'
	    line <- do many ws1; lineString
	    crLf
	    extraLines <- many extraHeaderLine
	    return (Header (map toLower name, concat (line:extraLines)))

extraHeaderLine = do sp <- ws1
		     line <- lineString
		     crLf
		     return (sp:line)

lineString = many (noneOf "\n\r")

headerBodySep = do crLf; return ()

body = many (do line <- many lineChar; crLf; return line)

message =
  do hs <- many header
     headerBodySep
     b <- body
     return (RawMessage hs b)

lookupHeader name msg =
  lookupInHeaders name (getHeaders msg)
lookupRawHeader name raw =
  lookupInHeaders name (rawHeaders raw)
lookupInHeaders name headers = g headers
  where g [] = Nothing
	g (Header (name', text):_) | name == name' = Just text
	g (_:rest) = g rest

parseHeader raw name deflt parser =
  fromMaybe deflt $
  do str <- lookupRawHeader name raw
     case parse parser name str of
       Right v -> return v
       Left _  -> Nothing

digestMessage :: RawMessage -> Message
digestMessage =
  digestMessage' (ContentType "text" "plain" [( "charset", "us-ascii")])

digestMessage' :: ContentType -> RawMessage -> Message
digestMessage' defcty raw =
  let cty = parseHeader raw
      	"content-type" defcty p_contentType
      cte = parseHeader raw
      	"content-transfer-encoding" (ContentTransferEncoding "7bit") p_contentTransferEncoding
      cdn = parseHeader raw
      	"content-disposition" (ContentDisposition "inline" []) p_contentDisposition
      cid = parseHeader raw
      	"content-id" (ContentID "(none)") p_contentID
      cdc = parseHeader raw
      	"content-description" (ContentDescription "(none)") p_contentDescription
      defaultMessage =
        Singlepart
      	{ getHeaders = rawHeaders raw
	, getLines = rawLines raw
	, getDecoded = decode cte (unlines (rawLines raw))
	, getContentType= cty
	, getContentTransferEncoding= cte
	, getContentDisposition= cdn
	}
  in
  case cty of
    ContentType "multipart" c_subtype c_parameters ->
      case lookup "boundary" c_parameters of
        Just boundary ->
	  let defcte 
	        | c_subtype == "digest" = 
		  ContentType "message" "rfc822" []
		| otherwise = 
		  ContentType "text" "plain" [("charset", "us-ascii")] in
	  Multipart
	  	{ getHeaders = rawHeaders raw
		, getLines = rawLines raw
		, getParts = map (digestMessage' defcte)
				 (splitBody boundary (rawLines raw))
		, getContentType= cty
		, getContentTransferEncoding= cte
		, getContentDisposition= cdn
		}
	_ ->
	  defaultMessage
    _ ->
      defaultMessage

splitBody boundary lines =
  g False lines (showChar '\n') []
  where 
    finish shower showers =
      reverse (map (\shower -> parseSuccessfully message "body part" (shower ""))
                   (shower:showers))
    g afterPreamble [] shower showers =
      finish shower showers
    g afterPreamble (xs : rest) shower showers =
      if innerboundary `isPrefixOf` xs 
      then if finalboundary `isPrefixOf` xs 
           then if afterPreamble 
	        then finish shower showers
		else finish (showChar '\n') []
	   else if afterPreamble
	        then g afterPreamble rest id (shower : showers)
		else g True rest (showChar '\n') []
      else
      g afterPreamble rest (shower . showString xs . showString "\n") showers
    innerboundary = '-':'-':boundary
    finalboundary = innerboundary ++ "--"

decode (ContentTransferEncoding "quoted-printable") rawlines =
  QuotedPrintable.decode rawlines
decode (ContentTransferEncoding "base64") rawlines =
  Base64.decode rawlines
-- "7bit", "8bit", "binary", and everything else
decode (ContentTransferEncoding _) rawlines =
  rawlines


parseSuccessfully p n inp =
  case parse p n inp of
    Left pError ->
      error (show pError)
    Right x ->
      x

-- |parse contents of Date field according to RFC2822
data DateTime2822 =
  DateTime2822 (Maybe DayOfWeek) Date2822 Time2822
  deriving Show
parseDateTime =
  do mdow <- option Nothing (try $ do fws
				      dow <- parseDayOfWeek
				      char ','
				      return (Just dow))
     date <- parseDate
     fws
     time <- parseTime
     return (DateTime2822 mdow date time)

type DayOfWeek = Int
parseDayOfWeek =
      (try (string "Mon") >> return (1 :: DayOfWeek))
  <|> (try (string "Tue") >> return 2)
  <|> (try (string "Wed") >> return 3)
  <|> (try (string "Thu") >> return 4)
  <|> (try (string "Fri") >> return 5)
  <|> (try (string "Sat") >> return 6)
  <|> (try (string "Sun") >> return 7)
  
data Date2822 =
  Date2822 Int Int Int
  deriving Show
parseDate =
  do d <- parseDay
     m <- parseMonth
     y <- parseYear
     return (Date2822 d m y)

parseDay =
  do fws
     d1 <- digit
     md2 <- option Nothing (digit >>= (return . Just))
     case md2 of
       Nothing ->
	 return (digitToInt d1)
       Just d2 ->
	 return (digitToInt d2 + 10 * digitToInt d1)

monthList = 
  ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
parseMonthName =
  foldr1 (<|>) (zipWith g monthList [1::Int ..])
  where 
    g mname mnr = try (string mname) >> return mnr
			 
parseMonth =
  do fws
     m <- parseMonthName
     fws
     return m

parseYear =
  do y1 <- digit
     y2 <- digit
     my3 <- option Nothing (digit >>= (return . Just))
     my4 <- option Nothing (digit >>= (return . Just))
     case (my3, my4) of
       (Just y3, Just y4) ->
	 return (1000 * digitToInt y1 + 100 * digitToInt y2 
		 + 10 * digitToInt y3 + digitToInt y4)
       -- interpretation of obs-year from RFC2822, 4.3
       (Just y3, Nothing) ->
         return (1900 + 100 * digitToInt y1 + 10 * digitToInt y2 + digitToInt y3)
       (Nothing, Nothing) ->
         let rawVal = 10 * digitToInt y1 + digitToInt y2 in
	 if rawVal < 50 
	 then return (2000 + rawVal)
	 else return (1900 + rawVal)
       _ ->
	 fail "parseYear"
data Time2822 =
  Time2822 TimeOfDay2822 Zone2822
  deriving Show
parseTime =
  do tod <- parseTimeOfDay
     fws
     zone <- parseZone
     return (Time2822 tod zone)

data TimeOfDay2822 =
  TimeOfDay2822 Int Int Int
  deriving Show
parseTimeOfDay =
  do hh <- parseTwoDigits
     char ':'
     mm <- parseTwoDigits
     ss <- option 0 (try $ do char ':'
			      parseTwoDigits)
     return (TimeOfDay2822 hh mm ss)

zoneInfoList =
  [( "UT",  (Zone2822 '+' 0 0))
  ,( "GMT", (Zone2822 '+' 0 0))
  ,( "EDT", (Zone2822 '-' 4 0))
  ,( "EST", (Zone2822 '-' 5 0))
  ,( "CDT", (Zone2822 '-' 5 0))
  ,( "CST", (Zone2822 '-' 6 0))
  ,( "MDT", (Zone2822 '-' 6 0))
  ,( "MST", (Zone2822 '-' 7 0))
  ,( "PDT", (Zone2822 '-' 7 0))
  ,( "PST", (Zone2822 '-' 8 0))
  ]

parseZoneInfo =
  foldr1 (<|>) (map g zoneInfoList)
  where 
    g (zname, zinfo) = try (string zname) >> return zinfo

data Zone2822 =
  Zone2822 Char Int Int
  deriving Show
parseZone =
  do sign <- oneOf "+-"
     hh <- parseTwoDigits
     mm <- parseTwoDigits
     return (Zone2822 sign hh mm)
  <|> parseZoneInfo
  -- anything else should be mapped to (Zone2822 '-' 0 0)

parseTwoDigits =
  do d1 <- digit
     d2 <- digit
     return (10 * digitToInt d1 + digitToInt d2)

