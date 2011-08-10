{-
Copyright (C) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}


module Maptest(tests) where
import Test.HUnit
import Data.Map.Utils
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
