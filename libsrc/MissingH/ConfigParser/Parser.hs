{- arch-tag: ConfigParser parser support
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

Parser support for "MissingH.ConfigParser".  This module is not intended to be
used directly by your programs.

Copyright (c) 2004 John Goerzen, jgoerzen\@complete.org
-}
module MissingH.ConfigParser.Parser
(
 parse_string,
       --satisfyG,
       --main
) where
import Text.ParserCombinators.Parsec
import MissingH.Str
import MissingH.ConfigParser.Lexer

----------------------------------------------------------------------
-- Exported funcs
----------------------------------------------------------------------

parse_string :: String -> [(String, [(String, String)])]
parse_string s = 
    detokenize "(string)" $ parse loken "(string)" s

----------------------------------------------------------------------
-- Private funcs
----------------------------------------------------------------------
detokenize fp l =
    let r = case l of
                   Left err -> error (show err)
                   Right reply -> reply
        in
        case runParser main () fp r of
                                    Left err -> error (show err)
                                    Right reply -> reply

main :: GenParser CPTok () [(String, [(String, String)])]
main =
    do {s <- sectionlist; return s}
    <|> try (do 
             o <- optionlist
             s <- sectionlist
             return $ ("DEFAULT", o) : s
            )
    <|> do {o <- optionlist; return $ [("DEFAULT", o)] }
    <?> "Error parsing config file tokens"
        
satisfyG :: (CPTok -> Bool) -> GenParser CPTok () CPTok
satisfyG f = tokenPrim (\c -> show [c])
                       (\pos _ _ -> pos)
                       (\c -> if f c then Just c else Nothing)

want :: (CPTok -> Maybe a) -> GenParser CPTok () a
want f = tokenPrim (\c -> show [c])
                   (\pos _ _ -> pos)
                   (\c -> f c)

sectionlist :: GenParser CPTok () [(String, [(String, String)])]
sectionlist = do {satisfyG (==EOFTOK); return []}
              <|> try (do 
                       s <- sectionhead
                       satisfyG (==EOFTOK)
                       return [(s, [])]
                      )
              <|> do
                  s <- section
                  sl <- sectionlist
                  return (s : sl)

section :: GenParser CPTok () (String, [(String, String)])
section = do {sh <- sectionhead; ol <- optionlist; return (sh, ol)}

sectionhead :: GenParser CPTok () String
sectionhead = 
    let wf (NEWSECTION x) = Just x
        wf _ = Nothing
        in
        do {s <- want wf; return $ strip s}

optionlist :: GenParser CPTok () [(String, String)]
optionlist =
    try (do {c <- coption; ol <- optionlist; return $ c : ol})
    <|> do {c <- coption; return $ [c]}

extensionlist :: GenParser CPTok () [String]
extensionlist =
    let wf (EXTENSIONLINE x) = Just x
        wf _ = Nothing
        in
        try (do {x <- want wf; l <- extensionlist; return $ x : l})
        <|> do {x <- want wf; return [x]}

coption :: GenParser CPTok () (String, String)
coption =
    let wf (NEWOPTION x) = Just x
        wf _ = Nothing
        in
        try (do 
             o <- want wf
             l <- extensionlist
             return (strip (fst o), valmerge ((snd o) : l ))
            )
        <|> do {o <- want wf; return $ (strip (fst o), strip (snd o))}

valmerge :: [String] -> String
valmerge vallist =
    let vl2 = map strip vallist
        in join "\n" vl2
