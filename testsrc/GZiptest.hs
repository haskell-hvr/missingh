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
import MissingH.Compression.Inflate
import System.IO
import MissingH.Either

mf fn exp conf = TestLabel fn $ TestCase $
                     do c <- readFile ("testsrc/gzfiles/" ++ fn)
                        assertEqual "" exp (conf c)

test_inflate = 
    let f fn exp conv = mf fn exp (conv . snd . forceEither . read_header) in
        [
         f "t1.gz" "Test 1" inflate_string
        ,f "t1.gz" ("Test 1",
                    "\x19\xf8\x27\x99\x06\x00\x00\x00") inflate_string_remainder
        ]

test_header =
    let f fn exp = mf fn exp (fst . forceEither . read_header)
        in
        [
         f "t1.gz" Header {method = 8, flags = 0, extra = Nothing,
                            filename = Nothing, comment = Nothing}
        ,f "empty.gz" Header {method = 8, flags = 8, extra = Nothing,
                              filename = Just "empty", 
                              comment = Nothing}
        ]

test_gunzip =
    let f fn exp = mf fn (Right exp) decompress
        in
        [
         f "t1.gz" "Test 1"
        ]

tests = TestList [TestLabel "inflate" (TestList test_inflate),
                  TestLabel "header" (TestList test_header),
                  TestLabel "gunzip" (TestList test_gunzip)

                 ]

