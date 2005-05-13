{- arch-tag: CSV tests main file
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

module Str.CSVtest(tests) where
import Test.HUnit
import MissingH.Str.CSV
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

