{- arch-tag: List tests main file
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

module Listtest(tests) where
import HUnit
import MissingH.List

test_delFromAL = 
    let f :: [(String, Int)] -> [(String, Int)] -> Assertion
        f inp exp = exp @=? (delFromAL inp "testkey") in
        do
                 f [] []
                 f [("one", 1)] [("one", 1)]
                 f [("1", 1), ("2", 2), ("testkey", 3)] [("1", 1), ("2", 2)]
                 f [("testkey", 1)] []
                 f [("testkey", 1), ("testkey", 2)] []
                 f [("testkey", 1), ("2", 2), ("3", 3)] [("2", 2), ("3", 3)]
                 f [("testkey", 1), ("2", 2), ("testkey", 3), ("4", 4)]
                   [("2", 2), ("4", 4)]

tests = TestList [TestLabel "delFromAL" (TestCase test_delFromAL)]



