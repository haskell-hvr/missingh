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

module MissingH.Network.FTP.Parser(
                       )
where

import Text.ParserCombinators.Parsec
import MissingH.Parsec

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
             first <- oneOf "123456789"
             remaining <- count 2 digit <?> "3-digit reply code"
             return (first : remaining)

specificCode :: Int -> Parser Int
specificCode exp = do
                   s <- string (show exp)
                   return (read s)

line :: Parser String
line = many (noneOf "\r\n")

----------------------------------------------------------------------
-- The parsers
----------------------------------------------------------------------

singleReplyLine :: Parser (Int, String)
singleReplyLine = do
                  x <- code
                  sp
                  text <- line
                  crlf
                  return (x, text)

expectedReplyLine :: Int -> Parser (Int, String)
expectedReplyLine expectedcode = do
                                 x <- specificCode expectedcode
                                 sp
                                 text <- line
                                 crlf
                                 return (x, text)

startOfMultiReply :: Parser (Int, String)
startOfMultiReply = do
                    x <- code
                    char '-'
                    text <- line
                    crlf
                    return (x, text)

multiReplyComponent :: Parser [String]
multiReplyComponent = try (do
                           notMatching codeString "found unexpected code"
                           thisLine <- line
                           remainder <- multiReplyComponent
                           return (thisLine : remainder)
                          )
                      <|> return []

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
