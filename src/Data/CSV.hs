{- arch-tag: CSV utilities
Copyright (c) 2005-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : Data.CSV
   Copyright  : Copyright (C) 2005-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Haskell Parsec parsers for comma-separated value (CSV) files.

Written by John Goerzen, jgoerzen\@complete.org
-}

module Data.CSV (csvFile, genCsvFile) where
import Text.ParserCombinators.Parsec
import Data.SeperatingValues.SeperatingValues

cell :: GenParser Char st String
cell = cellOfX ','

line :: GenParser Char st [String]
line = lineOfX ','

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
>import Data.String.CSV
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
genCsvFile inp = genXsvFile "," inp
