{-
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

module Maptest(tests) where
import Test.HUnit
import MissingH.Map
import Data.Map as M

test_flipM =
    let f inp exp = TestCase $ (M.fromList exp) @=? flipM (M.fromList inp) in
        [
         f ([]::[(Int,Int)]) ([]::[(Int,[Int])])
        ,f [("a", "b")] [("b", ["a"])]
        ,f [("a", "b"),
            ("c", "b"),
            ("d", "e"),
            ("b", "b")] [("b", ["c", "b", "a"]),
                         ("e", ["d"])]
        ]

test_flippedLookupM =
    let f item inp exp = TestCase $ exp @=? flippedLookupM item (M.fromList inp) in
        [
         f 'a' ([]::[(Char, Char)]) []
        ,f 'a' [("Test1", 'a'), ("Test2", 'b')] ["Test1"]
        ,f 'a' [("Test1", 'b'), ("Test2", 'b')] []
        ,f 'a' [("Test1", 'a'), ("Test2", 'a')] ["Test2", "Test1"]
        ]

tests = TestList [TestLabel "flipM" (TestList test_flipM),
                  TestLabel "flippedLookupM" (TestList test_flippedLookupM)
                 ]
