{- arch-tag: Tests for CRC-32 module
Copyright (C) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

module CRC32POSIXtest(tests) where
import Test.HUnit
import Data.Hash.CRC32.Posix

test_crc32 =
    let f msg inp exp = TestLabel msg $ TestCase $ assertEqual "" exp (crc32 inp) in
        [
         f "Empty" "" 4294967295,
         f "1" "1" 433426081,
         f "some numbers" "153141341309874102987412" 2083856642,
         f "Some text" "This is a test of the crc32 thing\n" 2449124888

        ]

tests = TestList [TestLabel "crc32" (TestList test_crc32)

                 ]

