{- arch-tag: Str tests main file
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

module Strtest(tests) where
import HUnit
import MissingH.Str
import Testutil
import Text.Regex

test_lstrip =
    mapassertEqual "lstrip" lstrip
                       [("", ""),
                        ("a", "a"),
                        (" a ", "a "),
                        ("  abas", "abas"),
                        ("\n\t fdsa", "fdsa"),
                        ("abc def", "abc def")]

test_rstrip =
    mapassertEqual "rstrip" rstrip
                   [("", ""),
                    ("a", "a"),
                    (" a ", " a"),
                    ("abas  ", "abas"),
                    ("fdsa \n\t", "fdsa"),
                    ("abc def", "abc def")]

test_strip =
    mapassertEqual "strip" strip
                   [("", ""),
                    ("a", "a"),
                    (" a ", "a"),
                    ("abas  ", "abas"),
                    ("  abas", "abas"),
                    ("asdf\n\t ", "asdf"),
                    ("\nbas", "bas"),
                    ("abc def", "abc def")]

test_splitRe =
    let f re inp exp = TestCase $ exp @=? splitRe (mkRegex re) inp
        in [
            f "foo" "" []
           ,f "foo" "foo" ["", ""]
           ,f "," "foo,bar,,baz," ["foo", "bar", "", "baz", ""]
           ,f "ba" ",foo,bar,,baz," [",foo,","r,,","z,"]
           ,f "," "" []
           ,f "," "," ["", ""]
           ]

test_subRe =
    let f re inp repl exp = TestCase $ exp @=? subRe (mkRegex re) inp repl
        in [
            f "foo" "" "bar" ""
           ,f "foo" "This is a foo test bar" "bar" "This is a bar test bar"
           ,f "foo" "Test foo bar" "x\\0x" "Test xfoox bar"
           ,f "(f)(o)o" "Test foo bar" "\\2\\1" "Test of bar"
           ,f "foo" "Test foo then foo bar" "" "Test  then  bar"
           ,f "foo" "Test foo bar" "x\\\\x" "Test x\\x bar"
           ]

tests = TestList [TestLabel "lstrip" (TestList test_lstrip),
                  TestLabel "rstrip" $ TestList test_rstrip,
                  TestLabel "strip" $ TestList test_strip,
                  TestLabel "subRe" $ TestList test_subRe,
                  TestLabel "splitRe" $ TestList test_splitRe
                  ]




