{- 
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

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

module Cmdtest(tests) where
import HUnit
import MissingH.Cmd
import System.IO

test_pOpen_Read = 
    let f cmd args result = TestLabel (cmd ++ " " ++ show args) $ TestCase $
                            do x <- pOpen ReadFromPipe cmd args rf
                               result @=? x
        rf h = do c <- hGetContents h
                  seq c (return c)
        in [
            f "echo" ["Hello."] "Hello\n"
           ]

tests = TestList [TestLabel "read" (TestList test_pOpen_Read)]


