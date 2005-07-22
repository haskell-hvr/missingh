{- arch-tag: FTP protocol parser for servers
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
   Module     : MissingH.Network.FTP.ParserServer
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: systems with networking

This module provides a parser that is used internally by
"MissingH.Network.FTP.Server".  You almost certainly do not want to use
this module directly.  Use "MissingH.Network.FTP.Server" instead.

Written by John Goerzen, jgoerzen\@complete.org

-}

module MissingH.Network.FTP.ParserServer(
                                         parseCommand
                                        )
where
import MissingH.Network.FTP.ParserClient
import Text.ParserCombinators.Parsec
import MissingH.Parsec
import MissingH.List
import MissingH.Bits
import MissingH.Str
import MissingH.Logging.Logger
import Network.Socket(SockAddr(..), PortNumber(..), inet_addr, inet_ntoa)
import System.IO(Handle, hGetContents)
import System.IO(hGetLine)
import Text.Regex
import Data.Word
import MissingH.Hsemail.Rfc2234(alpha)
import Data.Char

logit :: String -> IO ()
logit m = debugM "MissingH.Network.FTP.ParserServer" ("FTP received: " ++ m)

----------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------

word = many1 alpha

args :: Parser String
args = try (do char ' '
               r <- many anyChar
               eof 
               return r)
       <|> return ""
       

command :: Parser (String, String)
command = do
          x <- word
          y <- args
          eof
          return (map toUpper x, y)


parseCommand :: Handle -> IO (Either ParseError (String, String))
parseCommand h =
    do input <- hGetLine h
       return $ parse command "(unknown)" (rstrip input)
