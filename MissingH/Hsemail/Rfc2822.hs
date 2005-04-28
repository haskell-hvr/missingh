{- |
   Module      :  Text.ParserCombinators.Parsec.Rfc2822
   Copyright   :  (c) 2005-02-10 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  portable

   This module provides parsers for the grammar defined in
   RFC2822, \"Internet Message Format\",
   <http://www.faqs.org/rfcs/rfc2822.html>.

   /Please note:/ The module is a mess. I keep it around as
   a reminder that it needs to be rewritten, mostly.
   Nevertheless, some parsers -- like 'date_time', for
   example -- are genuinely useful.
-}

module MissingH.Hsemail.Rfc2822 where

import Text.ParserCombinators.Parsec
import Data.Char ( ord )
import Data.List ( intersperse )
import System.Time
import MissingH.Hsemail.Rfc2234
        hiding ( quoted_pair, quoted_string )

-- * Useful parser combinators

-- |@unfold@ @=@ @between (optional cfws) (optional cfws)@

unfold          :: CharParser a b -> CharParser a b
unfold           = between (optional cfws) (optional cfws)

-- |Construct a parser for a message header line from the
-- header's name and a parser for the body.

header          :: String -> CharParser a b -> CharParser a b
header n p       = let nameString = caseString (n ++ ":")
                   in
                   between nameString crlf p <?> (n ++ " header line")

-- |Like 'header', but allows the obsolete white-space rules.

obs_header      :: String -> CharParser a b -> CharParser a b
obs_header n p   = let nameString = caseString n >> many wsp >> char ':'
                   in
                   between nameString crlf p <?> ("obsolete " ++ n ++ " header line")


-- ** Primitive Tokens (section 3.2.1)

-- |Match any US-ASCII non-whitespace control character.

no_ws_ctl       :: CharParser a Char
no_ws_ctl       = satisfy (\c -> ord c `elem` ([1..8] ++ [11,12] ++ [14..31] ++ [127]))
                  <?> "US-ASCII non-whitespace control character"

-- |Match any US-ASCII character except for @\r@, @\n@.

text            :: CharParser a Char
text            = satisfy (\c -> ord c `elem` ([1..9] ++ [11,12] ++ [14..127]))
                  <?> "US-ASCII character (excluding CR and LF)"

-- |Match any of the RFC's \"special\" characters: @()\<\>[]:;\@,.\\\"@.

specials        :: CharParser a Char
specials        = oneOf "()<>[]:;@,.\\\""   <?> "one of ()<>[]:;@,.\\\""


-- ** Quoted characters (section 3.2.2)

-- |Match a \"quoted pair\". All characters matched by 'text' may be
-- quoted. Note that the parsers returns /both/ characters, the
-- backslash and the actual content.

quoted_pair     :: CharParser a String
quoted_pair     = do { char '\\'; r <- text; return ['\\',r] }
                  <?> "quoted pair"


-- ** Folding white space and comments (section 3.2.3)

-- |Match \"folding whitespace\". That is any combination of 'wsp' and
-- 'crlf' followed by 'wsp'.

fws             :: CharParser a String
fws             = do r <- many1 $ choice [ blanks, linebreak]
                     return (concat r)
    where
    blanks      = many1 wsp
    linebreak   = try $ do { r1 <- crlf; r2 <- blanks; return (r1 ++ r2) }

-- |Match any non-whitespace, non-control character except for \"@(@\",
-- \"@)@\", and \"@\\@\". This is used to describe the legal content of
-- 'comment's.
--
-- /Note/: This parser accepts 8-bit characters, even though this is
-- not legal according to the RFC. Unfortunately, 8-bit content in
-- comments has become fairly common in the real world, so we'll just
-- accept the fact.

ctext           :: CharParser a Char
ctext           = no_ws_ctl <|> satisfy (\c -> ord c `elem` ([33..39] ++ [42..91] ++ [93..126] ++ [128..255]))
                  <?> "any regular character (excluding '(', ')', and '\\')"

-- |Match a \"comments\". That is any combination of 'ctext',
-- 'quoted_pair's, and 'fws' between brackets. Comments may nest.

comment         :: CharParser a String
comment         = do char '('
                     r1 <- many ccontent
                     r2 <- option [] fws
                     char ')'
                     return ("(" ++ concat r1 ++ r2 ++ ")")
                  <?> "comment"
    where
    ccontent    = try $ do r1 <- option [] fws
                           r2 <- choice [many1 ctext, quoted_pair, comment]
                           return (r1 ++ r2)

-- |Match any combination of 'fws' and 'comments'.

cfws            :: CharParser a String
cfws            = do r <- many1 $ choice [ fws, comment ]
                     return (concat r)

-- ** Atom (section 3.2.4)

-- |Match any US-ASCII character except for control characters,
-- 'specials', or space. 'atom' and 'dot_atom' are made up of this.

atext           :: CharParser a Char
atext           = alpha <|> digit <|> oneOf "!#$%&'*+-/=?^_`{|}~"
                  <?> "US-ASCII character (excluding controls, space, and specials)"

-- |Match one or more 'atext' characters and skip any preceeding or
-- trailing 'cfws'.

atom            :: CharParser a String
atom            = unfold (many1 atext <?> "atom")

-- |Match 'dot_atom_text' and skip any preceeding or trailing 'cfws'.

dot_atom        :: CharParser a String
dot_atom        = unfold (dot_atom_text <?> "dot atom")

-- |Match two or more 'atext's interspersed by dots.

dot_atom_text   :: CharParser a String
dot_atom_text   = do r <- sepBy1 (many1 atext) (char '.')
                     return (concat (intersperse "." r))
                  <?> "dot atom content"


-- ** Quoted strings (section 3.2.5)

-- |Match any non-whitespace, non-control US-ASCII character except
-- for \"@\\@\" and \"@\"@\".

qtext           :: CharParser a Char
qtext           = no_ws_ctl <|> satisfy (\c -> ord c `elem` ([33] ++ [35..91] ++ [93..126]))
                  <?> "US-ASCII character (excluding '\\', and '\"')"

-- |Match either 'qtext' or 'quoted_pair'.

qcontent        :: CharParser a String
qcontent        = many1 qtext <|> quoted_pair
                  <?> "quoted string content"

-- |Match any number of 'qcontent' between double quotes. Any 'cfws'
-- preceeding or following the \"atom\" is skipped automatically.

quoted_string   :: CharParser a String
quoted_string   = unfold (do dquote
                             r1 <- many (do r1 <- option [] fws
                                            r2 <- qcontent
                                            return (r1 ++ r2))
                             r2 <- option [] fws
                             dquote
                             return ("\"" ++ concat r1 ++ r2 ++ "\""))
                  <?> "quoted string"


-- * Miscellaneous tokens (section 3.2.6)

-- |Match either 'atom' or 'quoted_string'.

word            :: CharParser a String
word            = atom <|> quoted_string     <?> "word"

-- |Match either one or more 'word's or an 'obs_phrase'.

phrase          :: CharParser a [String]
phrase          = {- many1 word <?> "phrase" <|> -} obs_phrase

-- |Match any non-whitespace, non-control US-ASCII character except
-- for \"@\\@\" and \"@\"@\".

utext           :: CharParser a Char
utext           = no_ws_ctl <|> satisfy (\c -> ord c `elem` [33..126])
                  <?> "regular US-ASCII character (excluding '\\', and '\"')"

-- |Match any number of 'utext' tokens.
--
-- \"Unstructured text\" is used in free text fields such as 'subject'.
-- Please note that any comments or whitespace that prefaces or
-- follows the actual 'utext' is /included/ in the returned string.

unstructured    :: CharParser a String
unstructured    = do r1 <- many (do r1 <- option [] fws
                                    r2 <- utext
                                    return (r1 ++ [r2]))
                     r2 <- option [] fws
                     return (concat r1 ++ r2)
                  <?> "unstructured text"


-- * Date and Time Specification (section 3.3)

-- |Parse a date and time specification of the form
--
-- >   Thu, 19 Dec 2002 20:35:46 +0200
--
-- where the weekday specification \"@Thu,@\" is optional. The parser
-- returns a 'CalendarTime', which is set to the appropriate values.
-- Note, though, that not all fields of 'CalendarTime' will
-- necessarily be set correctly! Obviously, when no weekday has been
-- provided, the parser will set this field to 'Monday' - regardless
-- of whether the day actually is a monday or not. Similarly, the day
-- of the year will always be returned as @0@. The timezone name will
-- always be empty: @\"\"@.
--
-- Nor will the 'date_time' parser perform /any/ consistency checking.
-- It will accept
--
-- >    40 Apr 2002 13:12 +0100
--
-- as a perfectly valid date.
--
-- In order to get all fields set to meaningful values, and in order
-- to verify the date's consistency, you will have to feed it into any
-- of the conversion routines provided in "System.Time", such as
-- 'toClockTime'. (When doing this, keep in mind that most functions
-- return /local time/. This will not necessarily be the time you're
-- expecting.)

date_time       :: CharParser a CalendarTime
date_time       = do wd <- option Monday (try (do wd <- day_of_week
                                                  char ','
                                                  return wd))
                     (y,m,d) <- date
                     fws
                     (td,z) <- time
                     optional cfws
                     return (CalendarTime y m d (tdHour td) (tdMin td) (tdSec td) 0 wd 0 "" z False)
                  <?> "date/time specification"

-- |This parser will match a 'day_name', optionally wrapped in folding
-- whitespace, or an 'obs_day_of_week' and return it's 'Day' value.

day_of_week     :: CharParser a Day
day_of_week     =     try (between (optional fws) (optional fws) day_name <?> "name of a day-of-the-week")
                  <|> obs_day_of_week

-- |This parser will the abbreviated weekday names (\"@Mon@\", \"@Tue@\", ...)
-- and return the appropriate 'Day' value.

day_name        :: CharParser a Day
day_name        =     do { caseString "Mon"; return Monday }
                  <|> do { try (caseString "Tue"); return Tuesday }
                  <|> do { caseString "Wed"; return Wednesday }
                  <|> do { caseString "Thu"; return Thursday }
                  <|> do { caseString "Fri"; return Friday }
                  <|> do { try (caseString "Sat"); return Saturday }
                  <|> do { caseString "Sun"; return Sunday }
                  <?> "name of a day-of-the-week"

-- |This parser will match a date of the form \"@dd:mm:yyyy@\" and return
-- a tripple of the form (Int,Month,Int) - corresponding to
-- (year,month,day).

date            :: CharParser a (Int,Month,Int)
date            = do d <- day
                     m <- month
                     y <- year
                     return (y,m,d)
                  <?> "date specification"

-- |This parser will match a four digit number and return it's integer
-- value. No range checking is performed.

year            :: CharParser a Int
year            = do y <- manyN 4 digit
                     return (read y :: Int)
                  <?> "year"

-- |This parser will match a 'month_name', optionally wrapped in
-- folding whitespace, or an 'obs_month' and return it's 'Month'
-- value.

month           :: CharParser a Month
month           =     try (between (optional fws) (optional fws) month_name <?> "month name")
                  <|> obs_month


-- |This parser will the abbreviated month names (\"@Jan@\", \"@Feb@\", ...)
-- and return the appropriate 'Month' value.

month_name      :: CharParser a Month
month_name      =     do { try (caseString "Jan"); return January }
                  <|> do { caseString "Feb"; return February }
                  <|> do { try (caseString "Mar"); return March }
                  <|> do { try (caseString "Apr"); return April }
                  <|> do { caseString "May"; return May }
                  <|> do { try (caseString "Jun"); return June }
                  <|> do { caseString "Jul"; return July }
                  <|> do { caseString "Aug"; return August }
                  <|> do { caseString "Sep"; return September }
                  <|> do { caseString "Oct"; return October }
                  <|> do { caseString "Nov"; return November }
                  <|> do { caseString "Dec"; return December }
                  <?> "month name"

-- |Match either an 'obs_day', or a one or two digit number and return it.

day             :: CharParser a Int
day             =  try (do { optional fws; r <- manyNtoM 1 2 digit; return (read r :: Int) }) <|> obs_day
                   <?> "day"

-- |This parser will match a 'time_of_day' specification followed by a
-- 'zone'. It returns the tuple (TimeDiff,Int) corresponding to the
-- return values of either parser.

time            :: CharParser a (TimeDiff,Int)
time            = do t <- time_of_day
                     fws
                     z <- zone
                     return (t,z)
                  <?> "time and zone specification"

-- |This parser will match a time-of-day specification of \"@hh:mm@\" or
-- \"@hh:mm:ss@\" and return the corrsponding time as a 'TimeDiff'.

time_of_day     :: CharParser a TimeDiff
time_of_day     = do h <- hour
                     char ':'
                     m <- minute
                     s <- option 0 (do { char ':'; second } )
                     return (TimeDiff 0 0 0 h m s 0)
                  <?> "time specification"

-- |This parser will match a two-digit number and return it's integer
-- value. No range checking is performed.

hour            :: CharParser a Int
hour            = do r <- count 2 digit
                     return (read r :: Int)
                  <?> "hour"

-- |This parser will match a two-digit number and return it's integer
-- value. No range checking is performed.

minute          :: CharParser a Int
minute          = do r <- count 2 digit
                     return (read r :: Int)
                  <?> "minute"

-- |This parser will match a two-digit number and return it's integer
-- value. No range checking takes place.

second          :: CharParser a Int
second          = do r <- count 2 digit
                     return (read r :: Int)
                  <?> "second"

-- |This parser will match a timezone specification of the form
-- \"@+hhmm@\" or \"@-hhmm@\" and return the zone's offset to UTC in
-- seconds as an integer. 'obs_zone' is matched as well.

zone            :: CharParser a Int
zone            = (    do char '+'
                          h <- hour
                          m <- minute
                          return (((h*60)+m)*60)
                   <|> do char '-'
                          h <- hour
                          m <- minute
                          return (-((h*60)+m)*60)
                   <?> "time zone"
                  )
                  <|> obs_zone


-- * Address Specification (section 3.4)

-- |Parse a single 'mailbox' or an address 'group' and return the
-- address(es).

address         :: CharParser a [String]
address         = try (do { r <- mailbox; return [r] }) <|> group
                  <?> "address"

-- |Parse a 'name_addr' or an 'addr_spec' and return the
-- address.

mailbox         :: CharParser a String
mailbox         = try name_addr <|> addr_spec
                  <?> "mailbox"

-- |Parse an 'angle_addr', optionally prefaced with a 'display_name',
-- and return the address.

name_addr       :: CharParser a String
name_addr       = do optional display_name
                     angle_addr
                  <?> "name address"

-- |Parse an 'angle_addr' or an 'obs_angle_addr' and return the address.

angle_addr      :: CharParser a String
angle_addr      = try (unfold (do char '<'
                                  r <- addr_spec
                                  char '>'
                                  return r)
                       <?> "angle address"
                      )
                  <|> obs_angle_addr

-- |Parse a \"group\" of addresses. That is a 'display_name', followed
-- by a colon, optionally followed by a 'mailbox_list', followed by a
-- semicolon. The found address(es) are returned - what may be none.
-- Here is an example:
--
-- >    parse group "" "my group: user1@example.org, user2@example.org;"
--
-- This input comes out as:
--
-- >    Right ["user1@example.org","user2@example.org"]

group           :: CharParser a [String]
group           = do display_name
                     char ':'
                     r <- option [] mailbox_list
                     unfold $ char ';'
                     return r
                  <?> "address group"

-- |Parse and return a 'phrase'.

display_name    :: CharParser a [String]
display_name    = phrase <?> "display name"

-- |Parse a list of 'mailbox' addresses, every two addresses being
-- separated by a comma, and return the list of found address(es).

mailbox_list    :: CharParser a [String]
mailbox_list    = sepBy mailbox (char ',') <?> "mailbox list"

-- |Parse a list of 'address' addresses, every two addresses being
-- separated by a comma, and return the list of found address(es).

address_list    :: CharParser a [String]
address_list    = do { r <-sepBy address (char ','); return (concat r) }
                  <?> "address list"


-- ** Addr-spec specification (section 3.4.1)

-- |Parse an \"address specification\". That is a 'local_part', followed
-- by an \"@\@@\" character, followed by a 'domain'. Return the complete
-- address as 'String', ignoring any whitespace or any comments.

addr_spec       :: CharParser a String
addr_spec       = do r1 <- local_part
                     char '@'
                     r2 <- domain
                     return (r1 ++ "@" ++ r2)
                  <?> "address specification"

-- |Parse and return a \"local part\" of an 'addr_spec'. That is either
-- a 'dot_atom' or a 'quoted_string'.

local_part      :: CharParser a String
local_part      = dot_atom <|> quoted_string
                  <?> "address' local part"

-- |Parse and return a \"domain part\" of an 'addr_spec'. That is either
-- a 'dot_atom' or a 'domain_literal'.

domain          :: CharParser a String
domain          = dot_atom <|> domain_literal
                  <?> "address' domain part"

-- |Parse a \"domain literal\". That is a \"@[@\" character, followed by
-- any amount of 'dcontent', followed by a terminating \"@]@\"
-- character. The complete string is returned verbatim.

domain_literal  :: CharParser a String
domain_literal  = unfold (do char '['
                             r <- many $ do { optional fws; dcontent }
                             optional fws
                             char ']'
                             return ("[" ++ concat r ++ "]"))
                  <?> "domain literal"

-- |Parse and return any characters that are legal in a
-- 'domain_literal'. That is 'dtext' or a 'quoted_pair'.

dcontent        :: CharParser a String
dcontent        = many1 dtext <|> quoted_pair
                  <?> "domain literal content"

-- |Parse and return any ASCII characters except \"@[@\", \"@]@\", and
-- \"@\\@\".

dtext           :: CharParser a Char
dtext           = no_ws_ctl
                  <|> satisfy (\c -> ord c `elem` ([33..90] ++ [94,127]))
                  <?> "character (excluding '[', ']', and '\\')"


-- * Overall message syntax (section 3.5)

-- |This data type repesents a parsed Internet Message as defined in
-- this RFC. It consists of an arbitrary number of header lines,
-- represented in the 'Field' data type, and a message body, which may
-- be empty.

data Message
    = Message [Field] String
      deriving (Show)

-- |Parse a complete message as defined by this RFC and it broken down
-- into the separate header fields and the message body. Header lines,
-- which contain syntax errors, will not cause the parser to abort.
-- Rather, these headers will appear as 'OptionalField's (which are
-- unparsed) in the resulting 'Message'. A message must be really,
-- really badly broken for this parser to fail.
--
-- This behaviour was chosen because it is impossible to predict what
-- the user of this module considers to be a fatal error;
-- traditionally, parsers are very forgiving when it comes to Internet
-- messages.
--
-- If you want to implement a really strict parser, you'll have to put
-- the appropriate parser together yourself. You'll find that this is
-- rather easy to do. Refer to the 'fields' parser for further details.

message         :: CharParser a Message
message         = do f <- fields
                     b <- option [] (do crlf
                                        b <- body
                                        return b)
                     return (Message f b)

-- |This parser will return a message body as specified by this RFC;
-- that is basically any number of 'text' characters, which may be
-- divided into separate lines by 'crlf'.

body            :: CharParser a String
body            = do r1 <- many (try (do line <- many text
                                         eol <- crlf
                                         return (line ++ eol)))
                     r2 <- many text
                     return (concat r1 ++ r2)


-- * Field definitions (section 3.6)

-- |This data type represents any of the header fields defined in this
-- RFC. Each of the various instances contains with the return value
-- of the corresponding parser.

data Field      = OptionalField       String String
                | From                [String]
                | Sender              String
                | ReturnPath          String
                | ReplyTo             [String]
                | To                  [String]
                | Cc                  [String]
                | Bcc                 [String]
                | MessageID           String
                | InReplyTo           [String]
                | References          [String]
                | Subject             String
                | Comments            String
                | Keywords            [[String]]
                | Date                CalendarTime
                | ResentDate          CalendarTime
                | ResentFrom          [String]
                | ResentSender        String
                | ResentTo            [String]
                | ResentCc            [String]
                | ResentBcc           [String]
                | ResentMessageID     String
                | ResentReplyTo       [String]
                | Received            ([(String,String)], CalendarTime)
                | ObsReceived         [(String,String)]
                deriving (Show)

-- |This parser will parse an arbitrary number of header fields as
-- defined in this RFC. For each field, an appropriate 'Field' value
-- is created, all of them making up the 'Field' list that this parser
-- returns.
--
-- If you look at the implementation of this parser, you will find
-- that it uses Parsec's 'try' modifier around /all/ of the fields.
-- The idea behind this is that fields, which contain syntax errors,
-- fall back to the catch-all 'optional_field'. Thus, this parser will
-- hardly ever return a syntax error -- what conforms with the idea
-- that any message that can possibly be accepted /should/ be.

fields          :: CharParser a [Field]
fields          = many (    try (do { r <- from; return (From r) })
                        <|> try (do { r <- sender; return (Sender r) })
                        <|> try (do { r <- return_path; return (ReturnPath r) })
                        <|> try (do { r <- reply_to; return (ReplyTo r) })
                        <|> try (do { r <- to; return (To r) })
                        <|> try (do { r <- cc; return (Cc r) })
                        <|> try (do { r <- bcc; return (Bcc r) })
                        <|> try (do { r <- message_id; return (MessageID r) })
                        <|> try (do { r <- in_reply_to; return (InReplyTo r) })
                        <|> try (do { r <- references; return (References r) })
                        <|> try (do { r <- subject; return (Subject r) })
                        <|> try (do { r <- comments; return (Comments r) })
                        <|> try (do { r <- keywords; return (Keywords r) })
                        <|> try (do { r <- orig_date; return (Date r) })
                        <|> try (do { r <- resent_date; return (ResentDate r) })
                        <|> try (do { r <- resent_from; return (ResentFrom r) })
                        <|> try (do { r <- resent_sender; return (ResentSender r) })
                        <|> try (do { r <- resent_to; return (ResentTo r) })
                        <|> try (do { r <- resent_cc; return (ResentCc r) })
                        <|> try (do { r <- resent_bcc; return (ResentBcc r) })
                        <|> try (do { r <- resent_msg_id; return (ResentMessageID r) })
                        <|> try (do { r <- received; return (Received r) })
                         -- catch all
                        <|> (do { (name,cont) <- optional_field; return (OptionalField name cont) })
                       )


-- ** The origination date field (section 3.6.1)

-- |Parse a \"@Date:@\" header line and return the date it contains a
-- 'CalendarTime'.

orig_date       :: CharParser a CalendarTime
orig_date       = header "Date" date_time


-- ** Originator fields (section 3.6.2)

-- |Parse a \"@From:@\" header line and return the 'mailbox_list'
-- address(es) contained in it.

from            :: CharParser a [String]
from            = header "From" mailbox_list

-- |Parse a \"@Sender:@\" header line and return the 'mailbox' address
-- contained in it.

sender          :: CharParser a String
sender          = header "Sender" mailbox

-- |Parse a \"@Reply-To:@\" header line and return the 'address_list'
-- address(es) contained in it.

reply_to        :: CharParser a [String]
reply_to        = header "Reply-To" address_list


-- ** Destination address fields (section 3.6.3)

-- |Parse a \"@To:@\" header line and return the 'address_list'
-- address(es) contained in it.

to              :: CharParser a [String]
to              = header "To" address_list

-- |Parse a \"@Cc:@\" header line and return the 'address_list'
-- address(es) contained in it.

cc              :: CharParser a [String]
cc              = header "Cc" address_list

-- |Parse a \"@Bcc:@\" header line and return the 'address_list'
-- address(es) contained in it.

bcc             :: CharParser a [String]
bcc             = header "Bcc" (try address_list <|> do { optional cfws; return [] })

-- ** Identification fields (section 3.6.4)

-- |Parse a \"@Message-Id:@\" header line and return the 'msg_id'
-- contained in it.

message_id      :: CharParser a String
message_id      = header "Message-ID" msg_id

-- |Parse a \"@In-Reply-To:@\" header line and return the list of
-- 'msg_id's contained in it.

in_reply_to     :: CharParser a [String]
in_reply_to     = header "In-Reply-To" (many1 msg_id)

-- |Parse a \"@References:@\" header line and return the list of
-- 'msg_id's contained in it.

references      :: CharParser a [String]
references      = header "References" (many1 msg_id)

-- |Parse a \"@message ID:@\" and return it. A message ID is almost
-- identical to an 'angle_addr', but with stricter rules about folding
-- and whitespace.

msg_id          :: CharParser a String
msg_id          = unfold (do char '<'
                             idl <- id_left
                             char '@'
                             idr <- id_right
                             char '>'
                             return ("<" ++ idl ++ "@" ++ idr ++ ">"))
                  <?> "message ID"

-- |Parse a \"left ID\" part of a 'msg_id'. This is almost identical to
-- the 'local_part' of an e-mail address, but with stricter rules
-- about folding and whitespace.

id_left         :: CharParser a String
id_left         = dot_atom_text <|> no_fold_quote
                  <?> "left part of an message ID"

-- |Parse a \"right ID\" part of a 'msg_id'. This is almost identical to
-- the 'domain' of an e-mail address, but with stricter rules about
-- folding and whitespace.

id_right        :: CharParser a String
id_right        = dot_atom_text <|> no_fold_literal
                  <?> "right part of an message ID"

-- |Parse one or more occurences of 'qtext' or 'quoted_pair' and
-- return the concatenated string. This makes up the 'id_left' of a
-- 'msg_id'.

no_fold_quote   :: CharParser a String
no_fold_quote   = do dquote
                     r <- many (many1 qtext <|> quoted_pair)
                     dquote
                     return ("\"" ++ concat r ++ "\"")
                  <?> "non-folding quoted string"

-- |Parse one or more occurences of 'dtext' or 'quoted_pair' and
-- return the concatenated string. This makes up the 'id_right' of a
-- 'msg_id'.

no_fold_literal :: CharParser a String
no_fold_literal = do char '['
                     r <- many (many1 dtext <|> quoted_pair)
                     char ']'
                     return ("\"" ++ concat r ++ "\"")
                     return ("[" ++ concat r ++ "]")
                  <?> "non-folding domain literal"


-- ** Informational fields (section 3.6.5)

-- |Parse a \"@Subject:@\" header line and return it's contents verbatim.

subject         :: CharParser a String
subject         = header "Subject" unstructured

-- |Parse a \"@Comments:@\" header line and return it's contents verbatim.

comments        :: CharParser a String
comments        = header "Comments" unstructured

-- |Parse a \"@Keywords:@\" header line and return the list of 'phrase's
-- found. Please not that each phrase is again a list of 'atom's, as
-- returned by the 'phrase' parser.

keywords        :: CharParser a [[String]]
keywords        = header "Keywords" (do r1 <- phrase
                                        r2 <- many (do char ','
                                                       r <- phrase
                                                       return r)
                                        return (r1:r2))


-- ** Resent fields (section 3.6.6)

-- |Parse a \"@Resent-Date:@\" header line and return the date it
-- contains as 'CalendarTime'.

resent_date     :: CharParser a CalendarTime
resent_date     = header "Resent-Date" date_time

-- |Parse a \"@Resent-From:@\" header line and return the 'mailbox_list'
-- address(es) contained in it.

resent_from     :: CharParser a [String]
resent_from     = header "Resent-From" mailbox_list


-- |Parse a \"@Resent-Sender:@\" header line and return the 'mailbox_list'
-- address(es) contained in it.

resent_sender   :: CharParser a String
resent_sender   = header "Resent-Sender" mailbox


-- |Parse a \"@Resent-To:@\" header line and return the 'mailbox'
-- address contained in it.

resent_to       :: CharParser a [String]
resent_to       = header "Resent-To" address_list

-- |Parse a \"@Resent-Cc:@\" header line and return the 'address_list'
-- address(es) contained in it.

resent_cc       :: CharParser a [String]
resent_cc       = header "Resent-Cc" address_list

-- |Parse a \"@Resent-Bcc:@\" header line and return the 'address_list'
-- address(es) contained in it. (This list may be empty.)

resent_bcc      :: CharParser a [String]
resent_bcc      = header "Resent-Bcc" (    try address_list
                                       <|> do optional cfws
                                              return []
                                      )
                  <?> "Resent-Bcc: header line"

-- |Parse a \"@Resent-Message-ID:@\" header line and return the 'msg_id'
-- contained in it.

resent_msg_id   :: CharParser a String
resent_msg_id   = header "Resent-Message-ID" msg_id


-- ** Trace fields (section 3.6.7)

return_path     :: CharParser a String
return_path     = header "Return-Path:" path

path            :: CharParser a String
path            = unfold (    do char '<'
                                 r <- choice [ try addr_spec, do { cfws; return [] } ]
                                 char '>'
                                 return ("<" ++ r ++ ">")
                          <|> obs_path
                         )
                  <?> "return path spec"

received        :: CharParser a ([(String,String)], CalendarTime)
received        = header "Received" (do r1 <- name_val_list
                                        char ';'
                                        r2 <- date_time
                                        return (r1,r2))

name_val_list   :: CharParser a [(String,String)]
name_val_list   = do optional cfws
                     many1 name_val_pair
                  <?> "list of name/value pairs"

name_val_pair   :: CharParser a (String,String)
name_val_pair   = do r1 <- item_name
                     cfws
                     r2 <- item_value
                     return (r1,r2)
                  <?> "a name/value pair"

item_name       :: CharParser a String
item_name       = do r1 <- alpha
                     r2 <- many $ choice [ char '-', alpha, digit ]
                     return (r1 : r2)
                  <?> "name of a name/value pair"

item_value      :: CharParser a String
item_value      = choice [ try (do { r <- many1 angle_addr; return (concat r) })
                         , try addr_spec
                         , try domain
                         , msg_id
                         , try atom
                         ]
                  <?> "value of a name/value pair"

-- ** Optional fields (section 3.6.8)

-- |Parse an arbitrary header field and return a tuple containing the
-- 'field_name' and 'unstructured' text of the header. The name will
-- /not/ contain the terminating colon.

optional_field  :: CharParser a (String,String)
optional_field  = do n <- field_name
                     char ':'
                     b <- unstructured
                     crlf
                     return (n,b)
                  <?> "optional (unspecified) header line"

-- |Parse and return an arbitrary header field name. That is one or
-- more 'ftext' characters.

field_name      :: CharParser a String
field_name      = many1 ftext <?> "header line name"

-- |Match and return any ASCII character except for control
-- characters, whitespace, and \"@:@\".

ftext           :: CharParser a Char
ftext           = satisfy (\c -> ord c `elem` ([33..57] ++ [59..126]))
                  <?> "character (excluding controls, space, and ':')"


-- * Miscellaneous obsolete tokens (section 4.1)

-- |Match the obsolete \"quoted pair\" syntax, which - unlike
-- 'quoted_pair' - allowed /any/ ASCII character to be specified when
-- quoted. The parser will return both, the backslash and the actual
-- character.

obs_qp          :: CharParser a String
obs_qp          = do char '\\'
                     c <- satisfy (\c -> ord c `elem` [0..127])
                     return ['\\',c]
                  <?> "any quoted US-ASCII character"

-- |Match the obsolete \"text\" syntax, which - unlike 'text' - allowed
-- \"carriage returns\" and \"linefeeds\". This is really weird; you
-- better consult the RFC for details. The parser will return the
-- complete string, including those special characters.

obs_text        :: CharParser a String
obs_text        = do r1 <- many lf
                     r2 <- many cr
                     r3 <- many (do r4 <- obs_char
                                    r5 <- many lf
                                    r6 <- many cr
                                    return (r4 : (r5 ++ r6)))
                     return (r1 ++ r2 ++ concat r3)

-- |Match and return the obsolete \"char\" syntax, which - unlike
-- 'character' - did not allow \"carriage return\" and \"linefeed\".

obs_char        :: CharParser a Char
obs_char        = satisfy (\c -> ord c `elem` ([0..9] ++ [11,12] ++ [14..127]))
                  <?> "any ASCII character except CR and LF"

-- |Match and return the obsolete \"utext\" syntax, which is identical
-- to 'obs_text'.

obs_utext       :: CharParser a String
obs_utext       = obs_text

-- |Match the obsolete \"phrase\" syntax, which - unlike 'phrase' -
-- allows dots between tokens.

obs_phrase      :: CharParser a [String]
obs_phrase      = do r1 <- word
                     r2 <- many $ choice [ word
                                         , string "."
                                         , do { cfws; return [] }
                                         ]
                     return (r1 : (filter (/=[]) r2))

-- |Match a  \"phrase list\" syntax and return the list of 'String's
-- that make up the phrase. In contrast to a 'phrase', the
-- 'obs_phrase_list' separates the individual words by commas. This
-- syntax is - as you will have guessed - obsolete.

obs_phrase_list :: CharParser a [String]
obs_phrase_list = do r1 <- many1 (do r <- option [] phrase
                                     unfold $ char ','
                                     return (filter (/=[]) r))
                     r2 <- option [] phrase
                     return (concat r1 ++ r2)
                  <|> phrase


-- * Obsolete folding white space (section 4.2)

-- |Parse and return an \"obsolete fws\" token. That is at least one
-- 'wsp' character, followed by an arbitrary number (including zero)
-- of 'crlf' followed by at least one more 'wsp' character.

obs_fws         :: CharParser a String
obs_fws         = do r1 <- many1 wsp
                     r2 <- many (do r3 <- crlf
                                    r4 <- many1 wsp
                                    return (r3 ++ r4))
                     return (r1 ++ concat r2)


-- * Obsolete Date and Time (section 4.3)

-- |Parse a 'day_name' but allow for the obsolete folding syntax.

obs_day_of_week :: CharParser a Day
obs_day_of_week = unfold day_name <?> "day-of-the-week name"

-- |Parse a 'year' but allow for a two-digit number (obsolete) and the
-- obsolete folding syntax.

obs_year        :: CharParser a Int
obs_year        = unfold (do r <- manyN 2 digit
                             return (normalize (read r :: Int)))
                  <?> "year"
    where
    normalize n
        | n <= 49   = 2000 + n
        | n <= 999  = 1900 + n
        | otherwise = n

-- |Parse a 'month_name' but allow for the obsolete folding syntax.

obs_month       :: CharParser a Month
obs_month       = between cfws cfws month_name <?> "month name"

-- |Parse a 'day' but allow for the obsolete folding syntax.

obs_day         :: CharParser a Int
obs_day         = unfold day <?> "day"

-- |Parse a 'hour' but allow for the obsolete folding syntax.

obs_hour        :: CharParser a Int
obs_hour        = unfold hour <?> "hour"

-- |Parse a 'minute' but allow for the obsolete folding syntax.

obs_minute      :: CharParser a Int
obs_minute      = unfold minute <?> "minute"

-- |Parse a 'second' but allow for the obsolete folding syntax.

obs_second      :: CharParser a Int
obs_second      = unfold second <?> "second"

-- |Match the obsolete zone names and return the appropriate offset.

obs_zone        :: CharParser a Int
obs_zone        = choice [ mkZone "UT"  0
                         , mkZone "GMT" 0
                         , mkZone "EST" (-5)
                         , mkZone "EDT" (-4)
                         , mkZone "CST" (-6)
                         , mkZone "CDT" (-5)
                         , mkZone "MST" (-7)
                         , mkZone "MDT" (-6)
                         , mkZone "PST" (-8)
                         , mkZone "PDT" (-7)
                         , do { r <- oneOf ['A'..'I']; return $ (ord r - 64) * 60*60 }  <?> "military zone spec"
                         , do { r <- oneOf ['K'..'M']; return $ (ord r - 65) * 60*60 }  <?> "military zone spec"
                         , do { r <- oneOf ['N'..'Y']; return $ -(ord r - 77) * 60*60 } <?> "military zone spec"
                         , do { char 'Z'; return 0 }                                    <?> "military zone spec"
                         ]
    where mkZone n o  = try $ do { string n; return (o*60*60) }


-- * Obsolete Addressing (section 4.4)

-- |This parser will match the \"obsolete angle address\" syntax. This
-- construct used to be known as a \"route address\" in earlier RFCs.
-- There are two differences between this construct and the
-- 'angle_addr': For one - as usual -, the obsolete form allows for
-- more liberal insertion of folding whitespace and comments.
--
-- Secondly, and more importantly, angle addresses used to allow the
-- (optional) specification of a \"route\". The newer version does not.
-- Such a routing address looks like this:
--
-- >    <@example1.org,@example2.org:simons@example.org>
--
-- The parser will return a tuple that - in case of the above address -
-- looks like this:
--
-- >    (["example1.org","example2.org"],"simons@example.org")
--
-- The first part contains a list of hosts that constitute the route
-- part. This list may be empty! The second part of the tuple is the
-- actual 'addr_spec' address.

obs_angle_addr  :: CharParser a String
obs_angle_addr  = unfold (do char '<'
                             _ <- option [] obs_route
                             addr <- addr_spec
                             char '>'
                             return addr)  -- TODO: route is lost here.
                  <?> "obsolete angle address"

-- |This parser parses the \"route\" part of 'obs_angle_addr' and
-- returns the list of 'String's that make up this route. Relies on
-- 'obs_domain_list' for the actual parsing.

obs_route       :: CharParser a [String]
obs_route       = unfold (do { r <- obs_domain_list; char ':'; return r })
                  <?> "route of an obsolete angle address"

-- |This parser parses a list of domain names, each of them prefaced
-- with an \"at\". Multiple names are separated by a comma. The list of
-- 'domain's is returned - and may be empty.

obs_domain_list :: CharParser a [String]
obs_domain_list = do char '@'
                     r1 <- domain
                     r2 <- many (do cfws <|> string ","
                                    optional cfws
                                    char '@'
                                    r <- domain
                                    return r)
                     return (r1 : r2)
                    <?> "route of an obsolete angle address"

-- |Parse the obsolete syntax of a 'local_part', which allowed for
-- more liberal insertion of folding whitespace and comments. The
-- actual string is returned.

obs_local_part  :: CharParser a String
obs_local_part  = do r1 <- word
                     r2 <- many (do string "."
                                    r <- word
                                    return ('.' : r))
                     return (r1 ++ concat r2)
                  <?> "local part of an address"

-- |Parse the obsolete syntax of a 'domain', which allowed for more
-- liberal insertion of folding whitespace and comments. The actual
-- string is returned.

obs_domain      :: CharParser a String
obs_domain      = do r1 <- atom
                     r2 <- many (do string "."
                                    r <- atom
                                    return ('.' : r))
                     return (r1 ++ concat r2)
                  <?> "domain part of an address"

-- |This parser will match the obsolete syntax for a 'mailbox_list'.
-- This one is quite weird: An 'obs_mbox_list' contains an arbitrary
-- number of 'mailbox'es - including none -, which are separated by
-- commas. But you may have multiple consecutive commas without giving
-- a 'mailbox'. You may also have a valid 'obs_mbox_list' that
-- contains /no/ 'mailbox' at all. On the other hand, you /must/ have
-- at least one comma.
--
-- So, this input is perfectly valid:
--
-- >    ","
--
-- But this one is - contrary to all intuition - not:
--
-- >    "simons@example.org"
--
-- Strange, isn't it?

obs_mbox_list   :: CharParser a [String]
obs_mbox_list   = do r1 <- many1 (try (do r <- option [] mailbox
                                          unfold $ char ','
                                          return r))
                     r2 <- option [] mailbox
                     return (filter (/=[]) (r1 ++ [r2]))
                  <?> "obsolete syntax for a list of mailboxes"

-- |This parser is identical to 'obs_mbox_list' but parses a list of
-- 'address'es rather than 'mailbox'es. The main difference is that an
-- 'address' may contain 'group's. Please note that as of now, the
-- parser will return a simple list of addresses; the grouping
-- information is lost.

obs_addr_list   :: CharParser a [String]
obs_addr_list   = do r1 <- many1 (try (do r <- option [] address
                                          optional cfws
                                          char ','
                                          optional cfws
                                          return (concat r)))
                     r2 <- option [] address
                     return (filter (/=[]) (r1 ++ r2))
                  <?> "obsolete syntax for a list of addresses"


-- * Obsolete header fields (section 4.5)

obs_fields      :: GenParser Char a [Field]
obs_fields      = many (    try (do { r <- obs_from; return (From r) })
                        <|> try (do { r <- obs_sender; return (Sender r) })
                        <|> try (do { r <- obs_return; return (ReturnPath r) })
                        <|> try (do { r <- obs_reply_to; return (ReplyTo r) })
                        <|> try (do { r <- obs_to; return (To r) })
                        <|> try (do { r <- obs_cc; return (Cc r) })
                        <|> try (do { r <- obs_bcc; return (Bcc r) })
                        <|> try (do { r <- obs_message_id; return (MessageID r) })
                        <|> try (do { r <- obs_in_reply_to; return (InReplyTo r) })
                        <|> try (do { r <- obs_references; return (References r) })
                        <|> try (do { r <- obs_subject; return (Subject r) })
                        <|> try (do { r <- obs_comments; return (Comments r) })
                        <|> try (do { r <- obs_keywords; return (Keywords [r]) })
                        <|> try (do { r <- obs_orig_date; return (Date r) })
                        <|> try (do { r <- obs_resent_date; return (ResentDate r) })
                        <|> try (do { r <- obs_resent_from; return (ResentFrom r) })
                        <|> try (do { r <- obs_resent_send; return (ResentSender r) })
                        <|> try (do { r <- obs_resent_to; return (ResentTo r) })
                        <|> try (do { r <- obs_resent_cc; return (ResentCc r) })
                        <|> try (do { r <- obs_resent_bcc; return (ResentBcc r) })
                        <|> try (do { r <- obs_resent_mid; return (ResentMessageID r) })
                        <|> try (do { r <- obs_resent_reply; return (ResentReplyTo r) })
                        <|> try (do { r <- obs_received; return (ObsReceived r) })
                         -- catch all
                        <|> (do { (name,cont) <- obs_optional; return (OptionalField name cont) })
                       )


-- ** Obsolete origination date field (section 4.5.1)

-- |Parse a 'date' header line but allow for the obsolete
-- folding syntax.

obs_orig_date   :: CharParser a CalendarTime
obs_orig_date   = obs_header "Date" date_time


-- ** Obsolete originator fields (section 4.5.2)

-- |Parse a 'from' header line but allow for the obsolete
-- folding syntax.

obs_from        :: CharParser a [String]
obs_from        = obs_header "From" mailbox_list

-- |Parse a 'sender' header line but allow for the obsolete
-- folding syntax.

obs_sender      :: CharParser a String
obs_sender      = obs_header "Sender" mailbox

-- |Parse a 'reply_to' header line but allow for the obsolete
-- folding syntax.

obs_reply_to    :: CharParser a [String]
obs_reply_to    = obs_header "Reply-To" mailbox_list


-- ** Obsolete destination address fields (section 4.5.3)

-- |Parse a 'to' header line but allow for the obsolete
-- folding syntax.

obs_to          :: CharParser a [String]
obs_to          = obs_header "To" address_list

-- |Parse a 'cc' header line but allow for the obsolete
-- folding syntax.

obs_cc          :: CharParser a [String]
obs_cc          = obs_header "Cc" address_list

-- |Parse a 'bcc' header line but allow for the obsolete
-- folding syntax.

obs_bcc         :: CharParser a [String]
obs_bcc         = header "Bcc" (    try address_list
                                    <|> do { optional cfws; return [] }
                               )


-- ** Obsolete identification fields (section 4.5.4)

-- |Parse a 'message_id' header line but allow for the obsolete
-- folding syntax.

obs_message_id  :: CharParser a String
obs_message_id  = obs_header "Message-ID" msg_id

-- |Parse an 'in_reply_to' header line but allow for the obsolete
-- folding and the obsolete phrase syntax.

obs_in_reply_to :: CharParser a [String]
obs_in_reply_to = obs_header "In-Reply-To" (do r <- many (    do {phrase; return [] }
                                                          <|> msg_id
                                                         )
                                               return (filter (/=[]) r))

-- |Parse a 'references' header line but allow for the obsolete
-- folding and the obsolete phrase syntax.

obs_references  :: CharParser a [String]
obs_references  = obs_header "References" (do r <- many (    do { phrase; return [] }
                                                         <|> msg_id
                                                        )
                                              return (filter (/=[]) r))

-- |Parses the \"left part\" of a message ID, but allows the obsolete
-- syntax, which is identical to a 'local_part'.

obs_id_left     :: CharParser a String
obs_id_left     = local_part <?> "left part of an message ID"

-- |Parses the \"right part\" of a message ID, but allows the obsolete
-- syntax, which is identical to a 'domain'.

obs_id_right    :: CharParser a String
obs_id_right    = domain <?> "right part of an message ID"



-- ** Obsolete informational fields (section 4.5.5)

-- |Parse a 'subject' header line but allow for the obsolete
-- folding syntax.

obs_subject     :: CharParser a String
obs_subject     = obs_header "Subject" unstructured

-- |Parse a 'comments' header line but allow for the obsolete
-- folding syntax.

obs_comments    :: CharParser a String
obs_comments    = obs_header "Comments" unstructured

-- |Parse a 'keywords' header line but allow for the obsolete
-- folding syntax. Also, this parser accepts 'obs_phrase_list'.

obs_keywords    :: CharParser a [String]
obs_keywords    = obs_header "Keywords" obs_phrase_list


-- ** Obsolete resent fields (section 4.5.6)

-- |Parse a 'resent_from' header line but allow for the obsolete
-- folding syntax.

obs_resent_from :: CharParser a [String]
obs_resent_from = obs_header "Resent-From" mailbox_list

-- |Parse a 'resent_sender' header line but allow for the obsolete
-- folding syntax.

obs_resent_send :: CharParser a String
obs_resent_send = obs_header "Resent-Sender" mailbox

-- |Parse a 'resent_date' header line but allow for the obsolete
-- folding syntax.

obs_resent_date :: CharParser a CalendarTime
obs_resent_date = obs_header "Resent-Date" date_time

-- |Parse a 'resent_to' header line but allow for the obsolete
-- folding syntax.

obs_resent_to   :: CharParser a [String]
obs_resent_to   = obs_header "Resent-To" mailbox_list

-- |Parse a 'resent_cc' header line but allow for the obsolete
-- folding syntax.

obs_resent_cc   :: CharParser a [String]
obs_resent_cc   = obs_header "Resent-Cc" mailbox_list

-- |Parse a 'resent_bcc' header line but allow for the obsolete
-- folding syntax.

obs_resent_bcc  :: CharParser a [String]
obs_resent_bcc  = obs_header "Bcc" (    try address_list
                                    <|> do { optional cfws; return [] }
                                   )

-- |Parse a 'resent_msg_id' header line but allow for the obsolete
-- folding syntax.

obs_resent_mid  :: CharParser a String
obs_resent_mid  = obs_header "Resent-Message-ID" msg_id

-- |Parse a @Resent-Reply-To@ header line but allow for the
-- obsolete folding syntax.

obs_resent_reply :: CharParser a [String]
obs_resent_reply = obs_header "Resent-Reply-To" address_list


-- ** Obsolete trace fields (section 4.5.7)

obs_return      :: CharParser a [Char]
obs_return       = obs_header "Return-Path" path

obs_received    :: CharParser a [(String, String)]
obs_received     = obs_header "Received" name_val_list

-- |Match 'obs_angle_addr'.

obs_path        :: CharParser a String
obs_path        = obs_angle_addr

-- |This parser is identical to 'optional_field' but allows the more
-- liberal line-folding syntax between the \"field_name\" and the \"field
-- text\".

obs_optional    :: CharParser a (String,String)
obs_optional    = do n <- field_name
                     many wsp
                     char ':'
                     b <- unstructured
                     crlf
                     return (n,b)
                  <?> "optional (unspecified) header line"
