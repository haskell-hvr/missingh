{-
Copyright (C) 2006-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}



module WildMatchtest(tests) where
import Test.HUnit
import System.Path.WildMatch
import Test.HUnit.Tools

test_wildCheckCase =
    let f patt name = TestCase $ assertBool (patt ++ "," ++ name ++ " was false")
                      (wildCheckCase patt name)
        f0 patt name = TestCase $ assertBool (patt ++ "," ++ name ++ " was true")
                       (not $ wildCheckCase patt name)
    in
    [f "asdf" "asdf",
     f "?*?" "abc",
     f "???*" "asd",
     f "*???" "asd",
     f "???" "asd",
     f "*" "asd",
     f "ab[cd]" "abc",
     f "ab[!de]" "abc",
     f0 "ab[de]" "abc",
     f0 "??" "a",
     f0 "a" "b",
     f "[\\]" "\\",
     f "[!\\]" "a",
     f0 "[!\\]" "\\",
     f0 "*.deb" "thedebianthing",
     f0 "a/*.foo" "testtmp/a/D"]
     
tests = TestList [TestLabel "wildCheckCase" (TestList test_wildCheckCase)]




