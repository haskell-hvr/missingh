{- arch-tag: FTP protocol parser
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
   Module     : MissingH.Network.FTP.Parser
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: systems with networking

This module provides a parser that is used internally by
"MissingH.Network.FTP.Client".  You almost certainly do not want to use
this module directly.  Use "MissingH.Network.FTP.Client" instead.

Written by John Goerzen, jgoerzen\@complete.org

-}

module MissingH.Network.FTP.Parser(parseReply, parseGoodReply,
                                  toPortString, fromPortString)
where

import Text.ParserCombinators.Parsec
import MissingH.Parsec
import MissingH.List
import MissingH.Bits
import Network.Socket(SockAddr(..), PortNumber(..))
-- import Control.Exception(Exception(PatternMatchFail), throw)

----------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------

crlf :: Parser String
crlf = string "\r\n" <?> "CRLF"

sp :: Parser Char
sp = char ' '

code :: Parser Int
code = do
       s <- codeString
       return (read s)

codeString :: Parser String
codeString = do
             first <- oneOf "123456789" <?> "3-digit reply code"
             remaining <- count 2 digit <?> "3-digit reply code"
             return (first : remaining)

specificCode :: Int -> Parser Int
specificCode exp = do
                   s <- string (show exp) <?> ("Code " ++ (show exp))
                   return (read s)

line :: Parser String
line = do
       x <- many (noneOf "\r\n")
       crlf
       -- return $ unsafePerformIO $ putStrLn ("line: " ++ x)
       return x

----------------------------------------------------------------------
-- The parsers
----------------------------------------------------------------------

singleReplyLine :: Parser (Int, String)
singleReplyLine = do
                  x <- code
                  sp
                  text <- line
                  return (x, text)

expectedReplyLine :: Int -> Parser (Int, String)
expectedReplyLine expectedcode = do
                                 x <- specificCode expectedcode
                                 sp
                                 text <- line
                                 return (x, text)

startOfMultiReply :: Parser (Int, String)
startOfMultiReply = do
                    x <- code
                    char '-'
                    text <- line
                    return (x, text)

multiReplyComponent :: Parser [String]
multiReplyComponent = (try (do
                            notMatching (do 
                                         codeString
                                         sp
                                        ) "found unexpected code"
                            thisLine <- line
                            -- return $ unsafePerformIO (putStrLn ("MRC: got " ++ thisLine))
                            remainder <- multiReplyComponent
                            return (thisLine : remainder)
                           )
                      ) <|> return []

multiReply :: Parser (Int, [String])
multiReply = try (do
                  x <- singleReplyLine
                  return (fst x, [snd x])
                 )
             <|> (do
                  start <- startOfMultiReply
                  component <- multiReplyComponent
                  end <- expectedReplyLine (fst start)
                  return (fst start, snd start : (component ++ [snd end]))
                 )

----------------------------------------------------------------------
-- The real code
----------------------------------------------------------------------

-- | Parse a FTP reply.  Returns a (result code, text) pair.

parseReply :: String -> (Int, [String])
parseReply input =
    case parse multiReply "(unknown)" input of
         Left err -> error (show err)
         Right reply -> reply

-- | Parse a FTP reply.  Returns a (result code, text) pair.
-- If the result code indicates an error, raise an exception instead
-- of just passing it back.

parseGoodReply :: String -> (Int, [String])
parseGoodReply input =
    let reply = parseReply input
        in
        if (fst reply) >= 400
        then error ((show (fst reply)) ++ ": " ++ (join "\n" (snd reply)))
        else reply

{- | Converts a socket address to a string suitable for a PORT command.

Example:

> toPortString (SockAddrInet (PortNum 0x1234) (0xaabbccdd)) ->
                              "170,187,204,221,18,52"
-}
toPortString :: SockAddr -> String
toPortString (SockAddrInet (PortNum port) hostaddr) =
    (genericJoin "," (getBytes hostaddr)) ++ "," ++ 
       (genericJoin "," (getBytes port))
toPortString _ = 
    error "toPortString only works on AF_INET addresses"

-- | Converts a port string to a socket address.  This is the inverse calculation of 'toPortString'.
fromPortString :: String -> SockAddr
fromPortString instr =
    let inbytes = split "," instr
        hostbytes = map read (take 4 inbytes)
        portbytes = map read (drop 4 inbytes)
        in
        SockAddrInet (PortNum (fromBytes portbytes)) (fromBytes hostbytes)
