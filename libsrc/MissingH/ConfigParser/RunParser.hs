{- arch-tag: ConfigParser parser running utilities
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
   Module     : MissingH.ConfigParser.RunParser
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Parser support for "MissingH.ConfigParser".  This module is not intended to be
used directly by your programs.

Copyright (c) 2004 John Goerzen, jgoerzen\@complete.org
-}

module MissingH.ConfigParser.RunParser(
                                       parse_string
                                      )
where

import Text.ParserCombinators.Parsec
import MissingH.ConfigParser.Parser
import MissingH.ConfigParser.Lexer

detokenize fp l =
    let r = case l of
                   Left err -> error (show err)
                   Right reply -> reply
        in
        case runParser main () fp r of
                                    Left err -> error (show err)
                                    Right reply -> reply

parse_string :: String -> [(String, [(String, String)])]
parse_string s = 
    detokenize "(string)" $ parse (many1 loken) "(string)" s