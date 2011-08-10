{- arch-tag: Parser for Debian control file
Copyright (c) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : System.Debian.ControlParser
   Copyright  : Copyright (C) 2004-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

This module provides various helpful utilities for dealing with Debian
files and programs.

Written by John Goerzen, jgoerzen\@complete.org
-}

module System.Debian.ControlParser(control, depPart)
    where

import Text.ParserCombinators.Parsec
import Data.String.Utils (split)

eol, extline :: GenParser Char st String
eol = (try (string "\r\n"))
      <|> string "\n" <?> "EOL"

extline = try (do char ' '
                  content <- many (noneOf "\r\n")
                  eol
                  return content )

entry :: GenParser Char st (String, String)
entry = do key <- many1 (noneOf ":\r\n")
           char ':'
           val <- many (noneOf "\r\n")
           eol
           exts <- many extline
           return (key, unlines ([val] ++ exts))

{- | Main parser for the control file -}
control :: CharParser a [(String, String)]
control = do many header
             retval <- many entry
             return retval

headerPGP, blankLine, header, headerHash :: GenParser Char st ()
headerPGP = do string "-----BEGIN PGP"
               manyTill (noneOf "\r\n") eol
               return ()
blankLine = do many (oneOf " \t")
               eol
               return ()
headerHash = do string "Hash: "
                manyTill (noneOf "\r\n") eol
                return ()
header = (try headerPGP) <|> (try blankLine) <|> (try headerHash)

{- | Dependency parser.

Returns (package name, Maybe version, arch list)

version is (operator, operand) -}
depPart :: CharParser a (String, (Maybe (String, String)), [String])
depPart = do packagename <- many1 (noneOf " (")
             many (char ' ')
             version <- (do char '('
                            op <- many1 (oneOf "<>=")
                            many (char ' ')
                            vers <- many1 (noneOf ") ")
                            many (char ' ')
                            char ')'
                            return $ Just (op, vers)
                        ) <|> return Nothing
             many (char ' ')
             archs <- (do char '['
                          t <- many1 (noneOf "]")
                          many (char ' ')
                          char ']'
                          return (split " " t)
                      ) <|> return []
             return (packagename, version, archs)
