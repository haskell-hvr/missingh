{- arch-tag: Path tests main file
Copyright (C) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE

-}

module Pathtest(tests) where
import Test.HUnit
import System.Path

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
