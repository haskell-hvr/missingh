{- |
   Module     : Data.TSV
   Copyright  : Copyright (C) 2005-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Haskell Parsec parsers for tab-separated value (TSV) files.

Written by: Aistis Raulinaitis, sheganinans@gmail.com
-}
module Data.TSV where

import Text.ParserCombinators.Parsec
import Data.SeperatingValues.SeperatingValues

cell :: GenParser Char st String
cell = cellOfX '\t'

line :: GenParser Char st [String]
line = lineOfX '\t'

{- | Parse a Tab-Separated Value (TSV) file.  The return value is a list of
lines; each line is a list of cells; and each cell is a String.

Please note that TSV files may have a different number of cells on each line.
Also, it is impossible to distinguish a TSV line that has a call with no data
from a TSV line that has no cells.

Here are some examples:

>Input (literal strings)          Parses As (Haskell String syntax)
>-------------------------------- ---------------------------------

>1	2	3                 [["1", "2", "3"]]
>
>l1                               [["l1"], ["l2"]]
>l2
>
> (empty line)                    [[""]]
>
>NQ	"Quoted"                  [["NQ", "Quoted"]]
>
>NQ	"Embedded""Quote"         [["NQ", "Embedded\"Quote"]]

To parse a String, you might use:

>import Text.ParserCombinators.Parsec
>import Data.String.TSV
>....
>parse tsvFile "" mystring

To parse a file, you might instead use:

>do result <- parseFromFile tsvFile "/path/to/file"

Please note that the result of parsing will be of type
(Either ParseError [[String]]).  A Left result indicates an error.
For more details, see the Parsec information.
-}

tsvFile :: CharParser st [[String]]
tsvFile = endBy line eol

{- | Generate TSV data for a file.  The resulting string can be
written out to disk directly. -}
genTsvFile :: [[String]] -> String
genTsvFile inp = genXsvFile "\t" inp
