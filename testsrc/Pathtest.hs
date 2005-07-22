{- arch-tag: Path tests main file
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

module Pathtest(tests) where
import Test.HUnit
import MissingH.Path

test_absNormPath =
    let f base p exp = TestLabel (show (base, p)) $ TestCase $ exp @=? absNormPath base p
        f2 = f "/usr/1/2" in
        [ 
         f "/" "" (Just "/")
        ,f "/usr/test" "" (Just "/usr/test")
        ,f "/usr/test" ".." (Just "/usr")
        ,f "/usr/1/2" "/foo/bar" (Just "/foo/bar")
        ,f2 "jack/./.." (Just "/usr/1/2")
        ,f2 "jack///../foo" (Just "/usr/1/2/foo")
        ,f2 "../bar" (Just "/usr/1/bar")
        ,f2 "../" (Just "/usr/1")
        ,f2 "../.." (Just "/usr")
        ,f2 "../../" (Just "/usr")
        ,f2 "../../.." (Just "/")
        ,f2 "../../../" (Just "/")
        ,f2 "../../../.." Nothing
        ]

test_secureAbsNormPath =
    let f base p exp = TestLabel (show (base, p)) $ TestCase $ exp @=? secureAbsNormPath base p
        f2 = f "/usr/1/2" in
        [ 
         f "/" "" (Just "/")
        ,f "/usr/test" "" (Just "/usr/test")
        ,f "/usr/test" ".." Nothing
        ,f "/usr/1/2" "/foo/bar" Nothing
        ,f "/usr/1/2" "/usr/1/2" (Just "/usr/1/2")
        ,f "/usr/1/2" "/usr/1/2/foo/bar" (Just "/usr/1/2/foo/bar")
        ,f2 "jack/./.." (Just "/usr/1/2")
        ,f2 "jack///../foo" (Just "/usr/1/2/foo")
        ,f2 "../bar" Nothing
        ,f2 "../" Nothing
        ,f2 "../.." Nothing
        ,f2 "../../" Nothing
        ,f2 "../../.." Nothing
        ,f2 "../../../" Nothing
        ,f2 "../../../.." Nothing
        ]

test_splitExt =
    let f inp exp = TestCase $ exp @=? splitExt inp in
        [
         f "" ("", "")
        ,f "/usr/local" ("/usr/local", "")
        ,f "../foo.txt" ("../foo", ".txt")
        ,f "../bar.txt.gz" ("../bar.txt", ".gz")
        ,f "foo.txt/bar" ("foo.txt/bar", "")
        ,f "foo.txt/bar.bz" ("foo.txt/bar", ".bz")
        ]

tests = TestList [TestLabel "splitExt" (TestList test_splitExt)
                 ,TestLabel "absNormPath" (TestList test_absNormPath)
                 ,TestLabel "secureAbsNormPath" (TestList test_secureAbsNormPath)
                 ]
