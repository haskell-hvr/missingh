{- arch-tag: Tests for GZip module
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

module GZiptest(tests) where
import HUnit
import MissingH.FileArchive.GZip
import System.IO

test_gunzip =
    let f fn exp = TestLabel fn $ TestCase $ 
                   do
                   fd <- openFile ("testsrc/gzfiles/" ++ fn) ReadMode
                   c <- hGetContents fd
                   assertEqual "" (Right exp) (decompress c)
        in
        [
         f "t1.gz" "Test 1"
        ]

tests = TestList [TestLabel "gunzip" (TestList test_gunzip)

                 ]

