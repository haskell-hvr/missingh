{- arch-tag: ConfigParser parser
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
   Module     : MissingH.ConfigParser.Parser
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Lexer support for "MissingH.ConfigParser".  This module is not intended to be
used directly by your programs.

Copyright (c) 2004 John Goerzen, jgoerzen\@complete.org
-}
module MissingH.ConfigParser.Parser
(
       -- -- * Temporary for testing
       --comment_chars, eol, optionsep, whitespace_chars, comment_line,
       --empty_line, sectheader_chars, sectheader, oname_chars, value_chars,
       --extension_line, optionkey, optionvalue, optionpair

       parse_string, parse_file, parse_handle, ParseOutput
) where

import Text.ParserCombinators.Parsec
import MissingH.Parsec
import MissingH.Str
import System.IO(Handle, hGetContents)

type ParseOutput = [(String, [(String, String)])]

comment_chars = oneOf "#;"
eol = string "\n" <|> string "\r\n" <|> string "\r" <?> "End of line"
optionsep = oneOf ":=" <?> "Option separator"
whitespace_chars = oneOf " \t" <?> "Whitespace"
comment_line = do skipMany whitespace_chars <?> "whitespace in comment"
                  comment_chars             <?> "start of comment"
                  (many1 $ noneOf "\r\n")   <?> "content of comment"
empty_line = many1 whitespace_chars         <?> "empty line"

ignore1 = eol 
          <|> try (do {comment_line; ignore1})
          <|> do {empty_line; ignore1}
ignorestuff = skipMany ignore1

sectheader_chars = noneOf "]\r\n"
sectheader = do ignorestuff
                char '['
                sname <- many1 $ sectheader_chars
                char ']'
                return sname
oname_chars = noneOf ":=\r\n"
value_chars = noneOf "\r\n"
extension_line = do ignorestuff
                    many1 whitespace_chars
                    c1 <- noneOf "\r\n#;"
                    remainder <- many value_chars
                    return (c1 : remainder)
                 <?> "extension line"

optionkey = many1 oname_chars           <?> "option key"
optionvalue = many1 value_chars         <?> "option value"
optionpair = do ignorestuff
                key <- optionkey
                optionsep
                value <- optionvalue
                return (key, value)
             <?> "option pair"

parsemain :: Parser [(String, [(String, String)])]
parsemain =
    sectionlist
    <|> try (do o <- optionlist
                s <- sectionlist
                return $ ("DEFAULT", o) : s
            )
    <|> do {o <- optionlist; return $ [("DEFAULT", o)]}
    <?> "High-level error parsing config file"

sectionlist = 
    do {eof; return []}
    <|> try (do
             s <- sectionhead
             eof
             return [(s, [])]
            )
    <|> do
        s <- section
        sl <- sectionlist
        return (s : sl)

section = do {sh <- sectionhead; ol <- optionlist; return (sh, ol)}


sectionhead = do {s <- sectheader; return $ strip s}
              <?> "start of section"

optionlist = 
    try (do {c <- coption; ol <- optionlist; return $ c : ol})
    <|> do {c <- coption; return $ [c]}

extensionlist =
    try (do {x <- extension_line; l <- extensionlist; return $ x : l})
    <|> do {x <- extension_line; return [x]}

coption =
    try (do o <- optionpair
            l <- extensionlist
            return (strip (fst o), valmerge ((snd o) : l))
        )
    <|> do {o <- optionpair; return $ (strip (fst o), strip (snd o))}
    <?> "an option"

valmerge :: [String] -> String
valmerge vallist =
    let vl2 = map strip vallist
        in join "\n" vl2

procparse fp l =
    case l of
           Left err -> error (show err)
           Right reply -> reply
----------------------------------------------------------------------
-- Exported funcs
----------------------------------------------------------------------

parse_string :: String -> ParseOutput
parse_string s = 
    procparse "(string)" $ parse parsemain "(string)" s

parse_file :: FilePath -> IO ParseOutput
parse_file f = do r <- parseFromFile parsemain f
                  return (procparse f r)

parse_handle :: Handle -> IO ParseOutput
parse_handle h =
    do s <- hGetContents h
       let r = parse parsemain (show h) s
       return $ procparse "(Handle)" r

