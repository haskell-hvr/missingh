{- arch-tag: ConfigParser tests main file
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

module ConfigParser.Maintest(tests) where
import HUnit
import MissingH.ConfigParser
import Testutil
import Control.Exception

test_basic =
    let p inp = readstring empty inp
        f msg inp exp conv = TestLabel msg $ TestCase $ assertEqual "" exp (conv (p inp))
        in
        [
         f "empty doc, no sections" "" [] sections,
         f "one empty line" "\n" [] sections,
         f "comment line only" "#foo bar" [] sections,
         f "comment line with \\n" "#foo bar\n" [] sections,
         f "one empty sect" "[emptysect]" ["emptysect"] sections,
         f "one empty sect w comment" "#foo bar\n[emptysect]\n" ["emptysect"]
           sections,
         f "assure comments not processed"
           "# [nonexistant]\n[emptysect]\n" ["emptysect"] sections,
         f "1 empty s w/comments"
           "#fo\n[Cemptysect]\n#asdf boo\n  \n  # fnonexistantg"
           ["Cemptysect"] sections,
         f "1 empty s, comments, EOL"
           "[emptysect]\n# [nonexistant]\n" ["emptysect"] sections,
         TestLabel "1 sec w/option" $ TestCase $
           do let cp = p "[sect1]\nfoo: bar\n"
              ["sect1"] @=? sections cp
              "bar" @=? get cp "sect1" "foo"
        , f "comments in option text"
            "[s1]\no1: v1#v2\n"
            "v1#v2" (\cp -> get cp "s1" "o1")
         
        ]

{-
disabled_test_basic =
    let f msg inp exp = TestLabel msg $ TestCase $ assertEqual "" exp (parse_string inp) in
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

disabled_test_asserts =
        do

        assertRaises "e test1" (ErrorCall "Lexer: \"(string)\" (line 1, column 5):\nunexpected \"\\n\"\nexpecting Option separator")
                      ([] @=? parse_string "#foo\nthis is bad data")

        assertRaises "e test2" (ErrorCall "Lexer: \"(string)\" (line 2, column 9):\nunexpected \"\\n\"\nexpecting Option separator")
                     ([] @=? parse_string "[sect1]\n#iiiiii \n  extensionline\n#foo")

disabled_test_extensionlines =
    let f inp exp = exp @=? parse_string inp in
        do
        f "[sect1]\nfoo: bar\nbaz: l1\n l2\n   l3\n# c\nquux: asdf"
          [("sect1", [("foo", "bar"),
                      ("baz", "l1\nl2\nl3"),
                      ("quux", "asdf")])]
-}
tests = TestList [TestLabel "test_basic" (TestList test_basic)]
