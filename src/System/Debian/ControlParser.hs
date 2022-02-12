{-# LANGUAGE Safe #-}

{- arch-tag: Parser for Debian control file
Copyright (c) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : System.Debian.ControlParser
   Copyright  : Copyright (C) 2004-2011 John Goerzen
   SPDX-License-Identifier: BSD-3-Clause

   Stability  : stable
   Portability: portable

This module provides various helpful utilities for dealing with Debian
files and programs.

Written by John Goerzen, jgoerzen\@complete.org
-}

module System.Debian.ControlParser(control, depPart)
    where

import safe Data.List.Utils ( split )
import safe Text.ParserCombinators.Parsec
    ( char,
      noneOf,
      oneOf,
      string,
      many1,
      manyTill,
      (<?>),
      (<|>),
      many,
      try,
      GenParser,
      CharParser )

eol, extline :: GenParser Char st String
eol = (try (string "\r\n"))
      <|> string "\n" <?> "EOL"

extline = try (do _ <- char ' '
                  content <- many (noneOf "\r\n")
                  _ <- eol
                  return content )

entry :: GenParser Char st (String, String)
entry = do key <- many1 (noneOf ":\r\n")
           _ <- char ':'
           val <- many (noneOf "\r\n")
           _ <- eol
           exts <- many extline
           return (key, unlines ([val] ++ exts))

{- | Main parser for the control file -}
control :: CharParser a [(String, String)]
control = do _ <- many header
             retval <- many entry
             return retval

headerPGP, blankLine, header, headerHash :: GenParser Char st ()
headerPGP = do _ <- string "-----BEGIN PGP"
               _ <- manyTill (noneOf "\r\n") eol
               return ()
blankLine = do _ <- many (oneOf " \t")
               _ <- eol
               return ()
headerHash = do _ <- string "Hash: "
                _ <- manyTill (noneOf "\r\n") eol
                return ()
header = (try headerPGP) <|> (try blankLine) <|> (try headerHash)

{- | Dependency parser.

Returns (package name, Maybe version, arch list)

version is (operator, operand) -}
depPart :: CharParser a (String, (Maybe (String, String)), [String])
depPart = do packagename <- many1 (noneOf " (")
             _ <- many (char ' ')
             version <- (do _ <- char '('
                            op <- many1 (oneOf "<>=")
                            _ <- many (char ' ')
                            vers <- many1 (noneOf ") ")
                            _ <- many (char ' ')
                            _ <- char ')'
                            return $ Just (op, vers)
                        ) <|> return Nothing
             _ <- many (char ' ')
             archs <- (do _ <- char '['
                          t <- many1 (noneOf "]")
                          _ <- many (char ' ')
                          _ <- char ']'
                          return (split " " t)
                      ) <|> return []
             return (packagename, version, archs)
