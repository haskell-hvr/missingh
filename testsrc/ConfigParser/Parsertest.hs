{- arch-tag: ConfigParser parser tests main file
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

module ConfigParser.Parsertest(tests) where
import HUnit
import MissingH.ConfigParser.Parser
import MissingH.ConfigParser.Types
import Testutil
import Control.Exception

test_basic =
    let f msg inp exp = TestLabel msg $ TestCase $ assertEqual "" (Right exp) (parse_string inp) in
        [
        f "empty string" "" []

        ,f "one empty line" "\n" []
        -- These two should go to OCaml
        ,f "one empty comment" "#" []
        ,f "one empty comment eol" "#\n" []
        ,f "one comment line" "#foo bar" []
        ,f "one comment line with eol" "#foo bar\n" []
        ,f "one empty section" "[emptysect]" [("emptysect", [])]
        ,f "one empty section w/eol" "[emptysect]\n" [("emptysect", [])]
        ,f "comment and empty sect noeol" "#foo bar\n[emptysect]"
           [("emptysect", [])]
        ,f "comment and empty sect" "#foo bar\n[emptysect]\n" [("emptysect", [])]
        ,f "comments2" "# [nonexistant]\n[emptysect]\n" [("emptysect", [])]
        ,f "comments3" "#fo\n[Cemptysect]\n#asdf boo\n  \n  # fnonexistantg"
          [("Cemptysect", [])]
        ,f "comments4" "[emptysect]\n# [nonexistant]\n" [("emptysect", [])]
        ,f "simple section" "[sect1]\nfoo: bar\n" [("sect1", [("foo", "bar")])]
        ,f "comments5" "\n#foo\n[sect1]\n\n#iiii \no1: v1\no2:  v2\no3: v3"
          [("sect1", [("o1", "v1"), ("o2", "v2"), ("o3", "v3")])]
        ,f "comments5ext" "\n#foo\n[sect1]\n\n#iiii \no1: v1\no2:  v2\n o3: v3"
          [("sect1", [("o1", "v1"), ("o2", "v2\no3: v3")])]
        ,f "comments5eol" "\n#foo\n[sect1]\n\n#iiii \no1: v1\no2:  v2\no3: v3\n"
          [("sect1", [("o1", "v1"), ("o2", "v2"), ("o3", "v3")])]

        ,f "default1" "v1: o1\n[sect1]\nv2: o2" [("DEFAULT", [("v1", "o1")]),
                                     ("sect1", [("v2", "o2")])]
        ,f "simple default" "foo: bar" [("DEFAULT", [("foo", "bar")])]
               ]

test_asserts =
    let f msg inp exp = TestLabel msg $ TestCase $ exp @=? parse_string inp in
        [
         f "e test1" "#foo\nthis is bad data"
                     (Left (ParseError "\"(string)\" (line 2, column 1):\nunexpected \"t\"\nexpecting end of input, whitespace, start of comment, empty line, start of section or option separator", "lexer"))
        ,f "e test2" "[sect1]\n#iiiiii \n  extensionline\n#foo"
                     (Left (ParseError "\"(string)\" (line 4, column 1):\nunexpected EXTENSIONLINE \"extensionline\"","parser"))
        ]
        
{-
        

        assertRaises "e test1" (ErrorCall "Lexer: \"(string)\" (line 1, column 5):\nunexpected \"\\n\"\nexpecting Option separator")
                      ([] @=? parse_string "#foo\nthis is bad data")

        assertRaises "e test2" (ErrorCall "Lexer: \"(string)\" (line 2, column 9):\nunexpected \"\\n\"\nexpecting Option separator")
                     ([] @=? parse_string "[sect1]\n#iiiiii \n  extensionline\n#foo")
-}

test_extensionlines =
    let f inp exp = (Right exp) @=? parse_string inp in
        do
        f "[sect1]\nfoo: bar\nbaz: l1\n l2\n   l3\n# c\nquux: asdf"
          [("sect1", [("foo", "bar"),
                      ("baz", "l1\nl2\nl3"),
                      ("quux", "asdf")])]

tests = TestList [TestLabel "test_basic" (TestList test_basic),
                  TestLabel "test_asserts" (TestList test_asserts),
                  TestLabel "test_extensionlines" (TestCase test_extensionlines)
                 ]