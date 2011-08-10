{-
Copyright (C) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}


module CRC32GZIPtest(tests) where
import Test.HUnit
import Data.Hash.CRC32.GZip

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

