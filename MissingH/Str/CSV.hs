{- arch-tag: CSV and TSV utilities
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

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
   Module     : MissingH.Str.CSV
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen,
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Haskell Parsec parsers for comma-separated value (CSV) files.

Written by John Goerzen, jgoerzen\@complete.org
-}

module MissingH.Str.CSV(csvFile, genCsvFile) where
import Text.ParserCombinators.Parsec
import Data.List

eol = (try $ string "\n\r") <|> (try $ string "\r\n") <|> string "\n" <|>
      string "\r" <?> "End of line"
cell = quotedcell <|> many (noneOf ",\n\r")
quotedchar = noneOf "\"" 
             <|> (try $ do string "\"\""
                           return '"'
                 )
quotedcell :: CharParser st String
quotedcell = do char '"'
                content <- many quotedchar
                char '"'
                return content
line = sepBy cell (char ',')

{- | Parse a Comma-Separated Value (CSV) file.  The return value is a list of
lines; each line is a list of cells; and each cell is a String.

Please note that CSV files may have a different number of cells on each line.
Also, it is impossible to distinguish a CSV line that has a call with no data
from a CSV line that has no cells.

Here are some examples:

>Input (literal strings)          Parses As (Haskell String syntax)
>-------------------------------- ---------------------------------
>1,2,3                            [["1", "2", "3"]]
>
>l1                               [["l1"], ["l2"]]
>l2
>
> (empty line)                    [[""]]
>
>NQ,"Quoted"                      [["NQ", "Quoted"]]
>
>NQ,"Embedded""Quote"             [["NQ", "Embedded\"Quote"]]

To parse a String, you might use:

>import Text.ParserCombinators.Parsec
>import MissingH.Str.CSV
>....
>parse csvFile "" mystring

To parse a file, you might instead use:

>do result <- parseFromFile csvFile "/path/to/file"

Please note that the result of parsing will be of type
(Either ParseError [[String]]).  A Left result indicates an error.
For more details, see the Parsec information.
-}

csvFile :: CharParser st [[String]]
csvFile = endBy line eol

{- | Generate CSV data for a file.  The resulting string can be
written out to disk directly. -}
genCsvFile :: [[String]] -> String
genCsvFile inp =
    unlines . map csvline $ inp
    where csvline :: [String] -> String
          csvline l = concat . intersperse "," . map csvcells $ l
          csvcells :: String -> String
          csvcells "" = ""
          csvcells c = '"' : convcell c ++ "\""
          convcell :: String -> String
          convcell c = concatMap convchar c
          convchar '"' = "\"\""
          convchar x = [x]
