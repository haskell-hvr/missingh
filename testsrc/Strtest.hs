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

tests = TestList [TestLabel "lstrip" (TestCase test_lstrip),
                  TestCase test_rstrip,
                  TestCase test_strip
                  ]




