{- arch-tag: Bits tests main file
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

module Bitstest(tests) where
import HUnit
import MissingH.Bits
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