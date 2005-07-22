{- arch-tag: MissingH.Network.FTP.Parser tests main file
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

module Network.FTP.Parsertest(tests) where
import Test.HUnit
import MissingH.Network.FTP.ParserClient
import Testutil
import Network.Socket

test_parseReply =
    let f inp exp = exp @=? parseReply inp in
        do
        f "200 Welcome to this server.\r\n" (200, ["Welcome to this server."])
        f "230-Foo\r\n230 Foo2\r\n" (230, ["Foo", "Foo2"])
        f "240-Foo\r\n240-Foo2\r\n240 Foo3\r\n" (240, ["Foo", "240-Foo2", "Foo3"])
        f "230-Test\r\nLine2\r\n 230 Line3\r\n230 Done\r\n"
          (230, ["Test", "Line2", " 230 Line3", "Done"])

{-
test_toPortString =
    let f inp exp = exp @=? toPortString inp in
        do
        f (SockAddrInet (PortNum 0x1234) 0xaabbccdd) "170,187,204,221,18,52"

test_fromPortString =
    let f inp exp = exp @=? case fromPortString inp of
                                 SockAddrInet (PortNum x) y -> (x, y)
                                 _ -> (0, 0)
        in
        do
        f "170,187,204,221,18,52" (0x1234, 0xaabbccdd)
-}
tests = TestList [TestLabel "parseReply" (TestCase test_parseReply)
                  --TestLabel "toPortString" (TestCase test_toPortString),
                  --TestLabel "fromPortString" (TestCase test_fromPortString)

                 ]
