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
import Testutil
import Control.Exception

test_basic =
    let f inp exp = exp @=? parse_string inp in
        do
        f "" []
        f "\n" []
        f "#foo bar" []
        f "#foo bar\n" []
        f "[emptysect]" [("emptysect", [])]
        f "#foo bar\n[emptysect]\n" [("emptysect", [])]
        f "# [nonexistant]\n[emptysect]\n" [("emptysect", [])]
        f "#fo\n[Cemptysect]\n#asdf boo\n  \n  # fnonexistantg"
          [("Cemptysect", [])]
        f "[emptysect]\n# [nonexistant]\n" [("emptysect", [])]
        f "[sect1]\nfoo: bar\n" [("sect1", [("foo", "bar")])]
        f "\n#foo\n[sect1]\n\n#iiii \no1: v1\no2:  v2\n o3: v3"
          [("sect1", [("o1", "v1"), ("o2", "v2"), ("o3", "v3")])]
        assertRaises (ErrorCall "Lexer: \"(string)\" (line 1, column 5):\nunexpected \"\\n\"\nexpecting Option separator")
                      (f "#foo\nthis is bad data" [])
        f "foo: bar" [("DEFAULT", [("foo", "bar")])]

tests = TestList [TestLabel "test_basic" (TestCase test_basic)
                 ]