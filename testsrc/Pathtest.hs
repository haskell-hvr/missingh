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
import HUnit
import MissingH.Path

test_splitExt =
    let f inp exp = exp @=? splitExt inp in
        do
        f "" ("", "")
        f "/usr/local" ("/usr/local", "")
        f "../foo.txt" ("../foo", ".txt")
        f "../bar.txt.gz" ("../bar.txt", ".gz")
        f "foo.txt/bar" ("foo.txt/bar", "")
        f "foo.txt/bar.bz" ("foo.txt/bar", ".bz")

tests = TestList [TestLabel "splitExt" (TestCase test_splitExt)
                 ]