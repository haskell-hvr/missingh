{- arch-tag: E-Mail Parsing Utility
Copyright (C) 2004 John Goerzen <jgoerzen@complete.org>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module     : MissingH.Email.Parser
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Parses an e-mail message

Written by John Goerzen, jgoerzen\@complete.org
-}

module MissingH.Email.Parser(mailParser)
where

import MissingH.Hsemail.Rfc2234(crlf)
import MissingH.Hsemail.Rfc2822 hiding (Message)
import MissingH.Wash.Mail.MailParser(RawMessage(..), digestMessage)
import MissingH.Wash.Mail.HeaderField(Header(..))
import qualified MissingH.Wash.Mail.Message
import Text.ParserCombinators.Parsec
import Control.Monad.Error
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Pos(newPos)

instance Error ParseError where
    noMsg = strMsg ""
    strMsg s = newErrorMessage (Message s) (newPos "" 0 0)

getHeaders :: CharParser a [(String, String)]
getHeaders =  many $ optional_field

parseMsg :: CharParser a ([(String, String)], String)
parseMsg = do f <- getHeaders
              crlf
              b <- body
              return (f, b)

{- | Parse a string as an e-mail, returning a
'MissingH.Wash.Mail.Message.Message' object. 

ParseError is defined in Text.ParserCombinators.Parsec.Error.
-}

mailParser :: String -> Either ParseError MissingH.Wash.Mail.Message.Message
mailParser s = do 
               p <- parse parseMsg ""  s
               let raw = RawMessage {rawHeaders = map Header (fst p),
                                     rawLines = lines (snd p)}
               return $ digestMessage raw

