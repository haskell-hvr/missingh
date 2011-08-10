{- arch-tag: CSV tests main file
Copyright (C) 2005-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE

-}

module Str.CSVtest(tests) where
import Test.HUnit
import Data.CSV
import Text.ParserCombinators.Parsec

test_csv =
    let f inp exp = TestLabel inp $ TestCase $ 
                    exp @=? case parse csvFile "" inp of
                                  Right x -> Right x
                                  Left y -> Left (show y)
        in [
        f "" (Right []),
        f "\n" (Right [[""]]),
        f "1,2,3\n" (Right [["1", "2", "3"]]),
        f "This is a,Test,Really\n" (Right [["This is a", "Test", "Really"]]),
        f "l1\nl2\n" (Right [["l1"], ["l2"]]),
        f "NQ,\"Quoted\"\n" (Right [["NQ", "Quoted"]]),
        f "1Q,\"\"\"\"\n" (Right [["1Q", "\""]]),
        f ",\"\"\n" (Right [["", ""]]),
        f "\"Embedded\"\"Quote\"\n" (Right [["Embedded\"Quote"]])
        ]

tests = TestList [TestLabel "csv" (TestList test_csv)]

