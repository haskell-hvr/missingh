{- |
   Module      :  Rfc2821
   Copyright   :  (c) 2004-10-12 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  portable

   This module exports (1) parsers for the grammars
   described in RFC2821, \"Simple Mail Transfer Protocol\",
   <http://www.faqs.org/rfcs/rfc2821.html>; and (2) an SMTP
   server state-machine as, well, suggested in the same
   document.
-}

module Rfc2821
  ( -- * Data Types for SMTP Commands
    SmtpCmd(..), Mailbox(..), nullPath, postmaster

    -- * Data Types for SMTP Replies
  , SmtpReply(..), SmtpCode(..), SuccessCode(..)
  , Category(..), reply

    -- * Command Parsers
  , SmtpParser, smtpCmd, smtpData, rset, quit
  , ehlo, mail, rcpt, send, soml, saml, vrfy
  , expn, help, noop, turn, helo

    -- * Argument Parsers
  , from_path, to_path, path, mailbox, local_part
  , domain, a_d_l, at_domain, element, name
  , dot_string, atom, dotnum, snum, number, word

    -- * SMTP Server State Machine
  , SmtpdFSM, SessionState(..), Event(..)
  , smtpdFSM, handleSmtpCmd

    -- * Utility Functions
  , fixCRLF
  )
  where

import Control.Exception ( assert )
import Control.Monad.State
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Data.List ( intersperse )
import Rfc2234

----- Smtp Parsers ---------------------------------------------------

-- |A parsed SMTP command. You can use 'show' and 'read' to
-- parse and display this data type, but you'll get nicer
-- error messages if you use the 'smtpCmd' parser directly.

data SmtpCmd
  = Helo String
  | Ehlo String
  | MailFrom Mailbox            -- ^ Might be 'nullPath'.
  | RcptTo Mailbox              -- ^ Might be 'postmaster'.
  | Data
  | Rset
  | Send Mailbox
  | Soml Mailbox
  | Saml Mailbox
  | Vrfy String
  | Expn String
  | Help String                 -- ^ Might be @[]@.
  | Noop                        -- ^ Optional argument ignored.
  | Quit
  | Turn
  | WrongArg String ParseError
      -- ^ When a valid command has been recognized, but the
      -- argument parser fails, then this type will be
      -- returned. The 'String' contains the name of the
      -- command (in all upper-case) and the 'ParseError'
      -- is, obviously, the error description.

instance Show SmtpCmd where
  show (Helo str)    = "HELO " ++ str
  show (Ehlo str)    = "EHLO " ++ str
  show (MailFrom mbox) = "MAIL FROM:" ++ show mbox
  show (RcptTo mbox) = "RCPT TO:" ++ show mbox
  show (Data)        = "DATA"
  show (Rset)        = "RSET"
  show (Send mbox)   = "SEND " ++ show mbox
  show (Soml mbox)   = "SOML " ++ show mbox
  show (Saml mbox)   = "SAML " ++ show mbox
  show (Vrfy str)    = "VRFY " ++ str
  show (Expn str)    = "EXPN " ++ str
  show (Noop)        = "NOOP"
  show (Quit)        = "QUIT"
  show (Turn)        = "TURN"
  show (Help t)
    | t == []        = "HELP"
    | otherwise      = "HELP " ++ t
  show (WrongArg str _) =
    "Syntax error in argument of " ++ str ++ "."

instance Read SmtpCmd where
  readsPrec _ = readWrapper smtpCmd
  readList    = error "reading [SmtpCmd] is not supported"

-- |The most generic e-mail address has the form:
-- @\<[\@route,...:]user\@domain\>@. This type, too,
-- supports 'show' and 'read'. Note that a \"shown\" address
-- is /always/ enclosed in angular brackets.

data Mailbox = Mailbox [String] String String
             deriving (Eq)

instance Show Mailbox where
  show (Mailbox [] [] []) = "<>"
  show (Mailbox [] "postmaster" []) = "<postmaster>"
  show (Mailbox p u d) = let
    route = concat . (intersperse ",") . (map ((:) '@')) $ p
    mbox  = u ++ "@" ++ d
    in if null route then "<" ++ mbox ++ ">"
                     else "<" ++ route ++ ":" ++ mbox ++ ">"

instance Read Mailbox where
  readsPrec _ = readWrapper (path <|> mailbox)
  readList    = error "reading [Mailbox] is not supported"

-- |@nullPath@ @=@ @'Mailbox' [] \"\" \"\" = \"\<\>\"@

nullPath :: Mailbox
nullPath = Mailbox [] [] []

-- |@postmaster@ @=@ @'Mailbox' [] \"postmaster\" \"\" = \"\<postmaster\>\"@

postmaster :: Mailbox
postmaster = Mailbox [] "postmaster" []

-- |An SMTP reply is a three-digit return code plus some
-- waste of bandwidth called \"comments\". This is what the
-- list of strings is for; one string per line in the reply.
-- 'show' will append an \"@\\r\\n@\" end-of-line marker to
-- each entry in that list, so that the resulting string is
-- ready to be sent back to the peer.
--
-- Here is an example:
--
-- > *Rfc2821> print $ Reply (Code Success MailSystem 0)
-- >                     ["worked", "like", "a charm" ]
-- > 250-worked
-- > 250-like
-- > 250 a charm
--
-- You might want to try it with an @[]@ and see what
-- great standard messages you will get. @:-)@
--
-- /TODO:/ Define 'read' for those as well.

data SmtpReply = Reply SmtpCode [String]

data SmtpCode = Code SuccessCode Category Int

data SuccessCode
  = Unused0
  | PreliminarySuccess
  | Success
  | IntermediateSuccess
  | TransientFailure
  | PermanentFailure
  deriving (Enum, Bounded, Eq, Ord, Show)

data Category
  = Syntax
  | Information
  | Connection
  | Unspecified3
  | Unspecified4
  | MailSystem
  deriving (Enum, Bounded, Eq, Ord, Show)

instance Show SmtpReply where
  show (Reply c@(Code suc cat _) []) =
    let msg = show suc ++ " in category " ++ show cat
    in
    show $ Reply c [msg]

  show (Reply code msg) =
    let prefixCon = show code ++ "-"
        prefixEnd = show code ++ " "
        fmt p l   = p ++ l ++ "\r\n"
        (x:xs) = reverse msg
        msgCon = map (fmt prefixCon) xs
        msgEnd = fmt prefixEnd x
        msg'   = reverse (msgEnd:msgCon)
    in
    concat msg'

instance Show SmtpCode where
  show (Code suc cat n) =
    assert (n >= 0 && n <= 9) $
      (show . fromEnum) suc ++ (show . fromEnum) cat ++ show n

-- |Construct a 'Reply'. Fails 'assert' if invalid numbers
-- are given.

reply :: Int -> Int -> Int -> [String] -> SmtpReply
reply suc c n msg =
  assert (suc >= 0 && suc <= 5) $
    assert (c >= 0 && c <= 5)   $
      assert (n >= 0 && n <= 9) $
        Reply (Code (toEnum suc) (toEnum c) n) msg

-- |The SMTP parsers defined here correspond to the commands
-- specified in RFC2821, so I won't document them
-- individually.

type SmtpParser st = GenParser Char st SmtpCmd

-- |This parser recognizes any of the SMTP commands defined
-- below.

smtpCmd :: SmtpParser st
smtpCmd = choice
          [ smtpData, rset, noop, quit, turn
          , helo, mail, rcpt, send, soml, saml
          , vrfy, expn, help, ehlo
          ]

-- |\"@data@\" was taken.
smtpData :: SmtpParser st
rset, quit, turn, helo, ehlo, mail :: SmtpParser st
rcpt, send, soml, saml, vrfy, expn :: SmtpParser st
help                               :: SmtpParser st

-- |May have an optional 'word' argument, but it is ignored.
noop :: SmtpParser st

smtpData = mkCmd0 "DATA" Data
rset = mkCmd0 "RSET" Rset
quit = mkCmd0 "QUIT" Quit
turn = mkCmd0 "TURN" Turn
helo = mkCmd1 "HELO" Helo     domain
ehlo = mkCmd1 "EHLO" Ehlo     domain
mail = mkCmd1 "MAIL" MailFrom from_path
rcpt = mkCmd1 "RCPT" RcptTo   to_path
send = mkCmd1 "SEND" Send     from_path
soml = mkCmd1 "SOML" Soml     from_path
saml = mkCmd1 "SAML" Saml     from_path
vrfy = mkCmd1 "VRFY" Vrfy     word
expn = mkCmd1 "EXPN" Expn     word

help = try (mkCmd0 "HELP" (Help [])) <|>
       mkCmd1 "HELP" Help (option [] word)

noop = try (mkCmd0 "NOOP" Noop) <|>
       mkCmd1 "NOOP" (\_ -> Noop) (option [] word)

from_path :: CharParser st Mailbox
from_path = do
  caseString "from:"
  (try (string "<>" >> return nullPath) <|> path)
                                <?> "from-path"

to_path :: CharParser st Mailbox
to_path = do
  caseString "to:"
  (try (caseString "<postmaster>" >> return postmaster)
     <|> path)                  <?> "to-path"

path :: CharParser st Mailbox
path = between (char '<') (char '>') (p <?> "path")
  where
  p = do r1 <- option [] (a_d_l >>= \r -> char ':' >> return r)
         (Mailbox _ l d) <- mailbox
         return (Mailbox r1 l d)

mailbox :: CharParser st Mailbox
mailbox = (do
  r1 <- local_part
  char '@'
  r2 <- domain
  return (Mailbox [] r1 r2))    <?> "mailbox"

local_part :: CharParser st String
local_part = (dot_string <|> quoted_string)
                                <?> "local-part"

domain :: CharParser st String
domain = tokenList element '.'  <?> "domain"

a_d_l :: CharParser st [String]
a_d_l = sepBy1 at_domain (char ',')
                                <?> "route-list"
at_domain :: CharParser st String
at_domain = (char '@' >> domain) <?> "at-domain"

element :: CharParser st String
element = (choice
  [ name
  , (char '#' >> number >>= \xs -> return ('#':xs))
                                <?> "#-literal"
  , (between (char '[') (char ']') dotnum >>= \xs ->
     return ("[" ++ xs ++ "]")) <?> "domain-literal"
  ]) <?> "domain element"

name :: CharParser st String
name = (do
  r1 <- alpha
  r2 <- many1 (alpha <|> digit <|> char '-')
  let r = r1 : r2
  if (last r == '-')
      then fail "name must not end with hyphen"
      else return r)            <?> "domain name"

dot_string :: CharParser st String
dot_string = tokenList atom '.' <?> "dot_string"

atom :: CharParser a String
atom = many1 atext              <?> "atom"
  where
  atext = alpha <|> digit <|> oneOf "!#$%&'*+-/=?^_`{|}~"

dotnum :: CharParser st String
dotnum = (do
  r1 <- snum
  char '.'
  r2 <- snum
  char '.'
  r3 <- snum
  char '.'
  r4 <- snum
  return (r1 ++ "." ++ r2 ++ "." ++ r3 ++ "." ++ r4))
                                <?> "IP address"

snum :: CharParser st String
snum = do
  r <- manyNtoM 1 3 hexdig
  if (read r :: Int) > 255
     then fail "IP address parts must be 0 <= x <= 255"
     else return r

number :: CharParser st String
number = many1 digit

-- |This is a useful addition: The parser accepts an 'atom'
-- or a 'quoted_string'.

word :: CharParser st String
word = (atom <|> (quoted_string >>= return . show))
       <?> "word or quoted-string"

-- |Helper function, which can be used to generate Parser-based
-- instances for the 'Read' class.

readWrapper :: GenParser tok () a -> [tok] -> [(a, [tok])]
readWrapper m x  = either (const []) (id) (parse m' "" x)
  where
  m' = do a <- m;
          res <- getInput
          return [(a,res)]


----- Smtp FSM -------------------------------------------------------

data SessionState
  = Unknown
  | HaveHelo
  | HaveMailFrom
  | HaveRcptTo
  | HaveData
  | HaveQuit
  deriving (Enum, Bounded, Eq, Ord, Show)

data Event
  = SayHelo       String
  | SayHeloAgain  String
  | SayEhlo       String
  | SayEhloAgain  String
  | SetMailFrom   Mailbox
  | AddRcptTo     Mailbox
  | StartData
  | NeedHeloFirst
  | NeedMailFromFirst
  | NeedRcptToFirst
  | NotImplemened
  | ResetState
  | SayOK
  | SeeksHelp     String
  | Shutdown
  | SyntaxErrorIn String
  | Unrecognized  String
  deriving (Eq, Show)

-- |Run like this: @'runState' (smtpdFSM \"noop\") HaveHelo@.

type SmtpdFSM = State SessionState Event

-- |Calling this function in 'HaveQuit' will fail an
-- assertion -- or, if 'assert' is disabled, return
-- 'Shutdown' again. Inputs must be terminated with 'crlf'.
-- See 'fixCRLF'.

smtpdFSM :: String -> SmtpdFSM
smtpdFSM str = either
                 (\_ -> return (Unrecognized str))
                 (handleSmtpCmd)
                 (parse smtpCmd "" str)

-- |Transform a command and a session state into an event
-- and a new session state.

handleSmtpCmd :: SmtpCmd -> SmtpdFSM
handleSmtpCmd cmd = get >>= \st -> match st cmd
  where
  match :: SessionState -> SmtpCmd -> SmtpdFSM
  match HaveQuit     _       = assert (False) (event Shutdown)
  match HaveData     _       = assert (False) (trans (HaveData, StartData))
  match    _  (WrongArg c _) = event (SyntaxErrorIn c)
  match    _        Quit     = trans (HaveQuit, Shutdown)
  match    _        Noop     = event SayOK
  match    _        Turn     = event NotImplemened

  match    _      (Send _)   = event NotImplemened
  match    _      (Soml _)   = event NotImplemened
  match    _      (Saml _)   = event NotImplemened
  match    _      (Vrfy _)   = event NotImplemened
  match    _      (Expn _)   = event NotImplemened
  match    _      (Help x)   = event (SeeksHelp x)

  match Unknown    Rset      = event SayOK
  match HaveHelo   Rset      = event SayOK
  match    _       Rset      = trans (HaveHelo, ResetState)

  match Unknown   (Helo x)   = trans (HaveHelo, SayHelo x)
  match    _      (Helo x)   = trans (HaveHelo, SayHeloAgain x)
  match Unknown   (Ehlo x)   = trans (HaveHelo, SayEhlo x)
  match    _      (Ehlo x)   = trans (HaveHelo, SayEhloAgain x)

  match Unknown (MailFrom _) = event NeedHeloFirst
  match    _    (MailFrom x) = trans (HaveMailFrom, SetMailFrom x)

  match Unknown  (RcptTo _)  = event NeedHeloFirst
  match HaveHelo (RcptTo _)  = event NeedMailFromFirst
  match    _     (RcptTo x)  = trans (HaveRcptTo, AddRcptTo x)

  match Unknown       Data   = event NeedHeloFirst
  match HaveHelo      Data   = event NeedMailFromFirst
  match HaveMailFrom  Data   = event NeedRcptToFirst
  match HaveRcptTo    Data   = trans (HaveData, StartData)


----- Utility --------------------------------------------------------

-- |Make the string 'crlf' terminated, no matter what.
-- \'@\\n@\' is expanded, otherwise 'crlf' is appended. Note
-- that if the strong was incorrectly terminated before, it
-- still is. So using this is safe, and useful when reading
-- with 'hGetLine', for example.

fixCRLF :: String -> String
fixCRLF ('\r' :'\n':[]) = fixCRLF []
fixCRLF (  x  :'\n':[]) = x : fixCRLF []
fixCRLF (  x  :  xs   ) = x : fixCRLF xs
fixCRLF      [ ]        = "\r\n"

----- Not exported ---------------------------------------------------

event :: Event -> SmtpdFSM
event = return

trans :: (SessionState, Event) -> SmtpdFSM
trans (st,e) = put st >> event e

-- Construct a parser for a command without arguments.
-- Expects 'crlf'!

mkCmd0 :: String -> a -> GenParser Char st a
mkCmd0 str cons = (do
  try (caseString str)
  skipMany wsp >> crlf
  return cons)                          <?> str

-- Construct a parser for a command with an argument which
-- the given parser can handle. The parsed result will be
-- applied to the given type constructor and returned.
-- Expects 'crlf'!

mkCmd1 :: String -> (a -> SmtpCmd) -> GenParser Char st a
       -> GenParser Char st SmtpCmd
mkCmd1 str cons p = do
  try (caseString str)
  wsp
  input <- getInput
  st <- getState
  let eol = skipMany wsp >> crlf
      p'  = (between (many wsp) eol p)  <?> str
      r   = runParser p' st "" input
  case r of
    Left e  -> return (WrongArg str e)
    Right a -> return (cons a)

-- @tokenList p '.'@ will parse a token of the form
-- \"@p.p@\", or \"@p.p.p@\", and so on. Used in 'domain'
-- and 'dot_string', for example.

tokenList :: GenParser Char st String -> Char
          -> GenParser Char st String
tokenList p c = do
  xs <- sepBy1 p (char c)
  return (concat (intersperse [c] xs))
