{- arch-tag: Parser for Debian control file
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
   Module     : MissingH.Debian.ControlParser
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

This module provides various helpful utilities for dealing with Debian
files and programs.

Written by John Goerzen, jgoerzen\@complete.org
-}

module MissingH.Debian.ControlParser(control, depPart)
    where
import Text.ParserCombinators.Parsec

eol = (try (string "\r\n"))
      <|> string "\n" <?> "EOL"

extline = try (do char ' '
                  content <- many (noneOf "\r\n")
                  eol
                  return content
              )

entry = do key <- many1 (noneOf ":\r\n")
           char ':'
           val <- many (noneOf "\r\n")
           eol
           exts <- many extline
           return (key, unlines ([val] ++ exts))

{- | Main parser for the control file -}
control = many entry

depPart = do packagename <- many1 (noneOf " (")
             version <- (do many (char ' ')
                            char '('
                            op <- many1 (oneOf "<>=")
                            many (char ' ')
                            vers <- many1 (noneOf ") ")
                            many (char ' ')
                            char ')'
                            return $ Just (op, vers)
                        ) <|> return Nothing
             return (packagename, version)

             