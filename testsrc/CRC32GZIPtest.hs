{- arch-tag: Tests for Gzip CRC-32 module
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

module CRC32GZIPtest(tests) where
import Test.HUnit
import MissingH.Checksum.CRC32.GZip

test_crcgzip =
    let f msg inp exp = TestLabel msg $ TestCase $ assertEqual "" exp (calc_crc32 inp) in
        [f "Simple" "Test 1" 0x9927f819
        ,f "Empty" "" 0x0
         --f "Empty" "" 4294967295,
         --f "1" "1" 433426081,
         --f "some numbers" "153141341309874102987412" 2083856642,
         --f "Some text" "This is a test of the crc32 thing\n" 2449124888

        ]

tests = TestList [TestLabel "crcgzip" (TestList test_crcgzip)

                 ]

