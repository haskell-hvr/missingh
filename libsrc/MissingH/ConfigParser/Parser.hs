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
 parse_string, parse_file, parse_handle, ParseOutput
       --satisfyG,
       --main
) where
import Text.ParserCombinators.Parsec
import MissingH.Str
import MissingH.ConfigParser.Lexer
import System.IO(Handle, hGetContents)
import MissingH.Parsec

type ParseOutput = [(String, [(String, String)])]

----------------------------------------------------------------------
-- Exported funcs
----------------------------------------------------------------------

parse_string :: String -> ParseOutput
parse_string s = 
    detokenize "(string)" $ parse loken "(string)" s

parse_file :: FilePath -> IO ParseOutput
parse_file f =
    do o <- parseFromFile loken f
       return $ detokenize f o

parse_handle :: Handle -> IO ParseOutput
parse_handle h =
    do s <- hGetContents h
       let o = parse loken (show h) s
       return $ detokenize (show h) o

----------------------------------------------------------------------
-- Private funcs
----------------------------------------------------------------------
detokenize fp l =
    let r = case l of
                   Left err -> error $ "Lexer: " ++ (show err)
                   Right reply -> reply
        in
        case runParser main () fp r of
                                    Left err -> error $ "Parser: " ++ (show err)
                                    Right reply -> reply

main :: GeneralizedTokenParser CPTok () ParseOutput
main =
    do {s <- sectionlist; return s}
    <|> try (do 
             o <- optionlist
             s <- sectionlist
             return $ ("DEFAULT", o) : s
            )
    <|> do {o <- optionlist; return $ [("DEFAULT", o)] }
    <?> "Error parsing config file tokens"
        
sectionlist :: GeneralizedTokenParser CPTok () ParseOutput
sectionlist = do {satisfyg (==EOFTOK); return []}
              <|> try (do 
                       s <- sectionhead
                       satisfyg (==EOFTOK)
                       return [(s, [])]
                      )
              <|> do
                  s <- section
                  sl <- sectionlist
                  return (s : sl)

section :: GeneralizedTokenParser CPTok () (String, [(String, String)])
section = do {sh <- sectionhead; ol <- optionlist; return (sh, ol)}

sectionhead :: GeneralizedTokenParser CPTok () String
sectionhead = 
    let wf (NEWSECTION x) = Just x
        wf _ = Nothing
        in
        do {s <- tokeng wf; return $ strip s}

optionlist :: GeneralizedTokenParser CPTok () [(String, String)]
optionlist = many1 coption

coption :: GeneralizedTokenParser CPTok () (String, String)
coption =
    let wf (NEWOPTION x) = Just x
        wf _ = Nothing
        wfx (EXTENSIONLINE x) = Just x
        wfx _ = Nothing
        in
        do o <- tokeng wf
           l <- many $ tokeng wfx
           return (strip (fst o), valmerge ((snd o) : l))

valmerge :: [String] -> String
valmerge vallist =
    let vl2 = map strip vallist
        in join "\n" vl2
