{- |
   Module     : Data.SeperatingValues.SeperatingValues
   Copyright  : Copyright (C) 2005-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Misc/Helper Haskell Parsec parsers for any(X)-separated values (XSV) files.

Written by: Aistis Raulinaitis, sheganinans@gmail.com
-}

module Data.SeperatingValues.SeperatingValues(eol, cellOfX, quotedchar, lineOfX, genXsvFile) where

import Text.ParserCombinators.Parsec
import Data.List (intersperse)

eol :: forall st. GenParser Char st String
eol = (try $ string "\n\r") <|> (try $ string "\r\n") <|> string "\n" <|>
      string "\r" <?> "End of line"

cellOfX :: Char -> GenParser Char st String
cellOfX x = quotedcell <|> many (noneOf (x : "\n\r"))

quotedchar :: GenParser Char st Char
quotedchar = noneOf "\""
             <|> (try $ do string "\"\""
                           return '"'
                 )
quotedcell :: CharParser st String
quotedcell = do char '"'
                content <- many quotedchar
                char '"'
                return content

lineOfX :: Char -> GenParser Char st [String]
lineOfX x = sepBy (cellOfX x) (char x)

genXsvFile :: String -> [[String]] -> String
genXsvFile x inp =
    unlines . map xsvline $ inp
    where xsvline :: [String] -> String
          xsvline l = concat . intersperse x . map xsvcells $ l
          xsvcells :: String -> String
          xsvcells "" = ""
          xsvcells c = '"' : convcell c ++ "\""
          convcell :: String -> String
          convcell c = concatMap convchar c
          convchar '"' = "\"\""
          convchar x = [x]
