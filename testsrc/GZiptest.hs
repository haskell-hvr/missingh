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
import Data.List

mf fn exp conf = TestLabel fn $ TestCase $
                     do c <- readFile ("testsrc/gzfiles/" ++ fn)
                        assertEqual "" exp (conf c)

{-
import MissingH.FileArchive.GZip
import System.IO
import MissingH.Either

main = do
       c <- hGetContents stdin
       let x = snd . forceEither . read_header $ c
       putStr x

test_bunches =
    let f fn exp conv = mf fn exp (conv . snd . forceEither . read_header)
        f2 c = let fn = "t/z" ++ (show c) ++ ".gz" in
                   f fn c (length . inflate_string)
        in
        map f2 [0..1000]
-}
test_inflate = 
    let f fn exp conv = mf fn exp (conv . snd . forceEither . read_header) in
        [
         f "t1.gz" "Test 1" inflate_string
        ,f "t1.gz" 6 (length . inflate_string)
        ,f "t1.gz" ("Test 1",
                    "\x19\xf8\x27\x99\x06\x00\x00\x00") inflate_string_remainder
        ,f "empty.gz" "" inflate_string
        ,f "zeros.gz" 10485760 (length . inflate_string)
        --,f "zeros.gz" (replicate (10 * 1048576) '\0') inflate_string
        ]

test_header =
    let f fn exp = mf fn exp (fst . forceEither . read_header)
        in
        [
         f "t1.gz" Header {method = 8, flags = 0, extra = Nothing,
                            filename = Nothing, comment = Nothing,
                          mtime = 1102111446, xfl = 2, os = 3}
        ,f "empty.gz" Header {method = 8, flags = 8, extra = Nothing,
                              filename = Just "empty", 
                              comment = Nothing,
                             mtime = 1102127257, xfl = 0, os = 3}
        ]

test_gunzip =
    let f fn exp = mf fn (Right exp) decompress
        in
        [
         f "t1.gz" ("Test 1", True)
        ]

tests = TestList [TestLabel "inflate" (TestList test_inflate),
                  TestLabel "header" (TestList test_header),
--                  TestLabel "bunches" (TestList test_bunches),
                  TestLabel "gunzip" (TestList test_gunzip)

                 ]

