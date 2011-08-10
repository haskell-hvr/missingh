{-
Copyright (C) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}


module Bitstest(tests) where
import Test.HUnit
import Data.Bits.Utils
import Data.Word

test_fromBytes =
    let f :: [Word32] -> Word32 -> Test
        f inp exp = TestCase $ exp @=? fromBytes inp in
        [
         f [] 0
        ,f [0] 0
        ,f [1] 1
        ,f [0xff, 0] 0xff00
        ,f [0x0, 0xff] 0xff
        ,f [0x12, 0x34, 0x56, 0x78] 0x12345678
        ,f [0xff, 0xff, 0xff, 0xff] 0xffffffff
        ,f [0xff, 0, 0, 0] 0xff000000
        ]

test_getBytes =
    let f :: Word32 -> [Word32] -> Test
        f inp exp = TestCase $ exp @=? getBytes inp in
        [
         f 0 [0, 0, 0, 0]
        ,f 0x1200 [0, 0, 0x12, 0]
        ,f 0x0012 [0, 0, 0, 0x12]
        ,f 0xffffffff [0xff, 0xff, 0xff, 0xff]
        ,f 0x12345678 [0x12, 0x34, 0x56, 0x78]
        ,f 0xf0000000 [0xf0, 0, 0, 0]
        ]

tests = TestList [TestLabel "getBytes" (TestList test_getBytes),
                  TestLabel "fromBytes" (TestList test_fromBytes)
                 ]
