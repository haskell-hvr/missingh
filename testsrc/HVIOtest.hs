{- arch-tag: HVIO tests main file
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

module HVIOtest(tests) where
import HUnit
import MissingH.IO.HVIO
import Testutil
import System.IO
import System.IO.Error
import Control.Exception

ioeq :: (Show a, Eq a) => a -> IO a -> Assertion
ioeq exp inp = do x <- inp
                  exp @=? x

test_MemoryBuffer =
    let f inp testfunc = TestLabel inp $ TestCase $ do x <- newMemoryBuffer inp mbDefaultCloseFunc
                                                       testfunc x
        in
        [
         f "" (\x -> do True `ioeq` vIsOpen x
                        assertRaises "eof error" (IOException $ mkIOError eofErrorType "" Nothing Nothing) (vGetChar x)
                        vPutStrLn x "Line1"
                        vPutStrLn x "Line2"
                        vRewind x
                        "Line1" `ioeq` vGetLine x
                        "Line2" `ioeq` vGetLine x
                        12 `ioeq` vTell x
                        vSeek x AbsoluteSeek 1
                        "ine1" `ioeq` vGetLine x
                        vSeek x RelativeSeek (-3)
                        "e1" `ioeq` vGetLine x
                        vSeek x SeekFromEnd (-3)
                        "e2" `ioeq` vGetLine x
                        vSeek x SeekFromEnd 0
                        vPutChar x 'c'
                        assertRaises "eof error" (IOException $ mkIOError eofErrorType "" Nothing Nothing) (vGetLine x)
                        vRewind x
                        "Line1\nLine2\nc" `ioeq` vGetContents x
              )
        ]

test_StreamReader =
    let f inp testfunc = TestLabel inp $ TestCase $ do x <- newStreamReader inp
                                                       testfunc x
        in 
        [
         f "" (\x -> do True `ioeq` vIsEOF x
                        True `ioeq` vIsOpen x
                        assertRaises "eof error" (IOException $ mkIOError eofErrorType "" Nothing Nothing) (vGetChar x)
                        vClose x
                        False `ioeq` vIsOpen x
                        
              )
        ,f "abcd" (\x -> do False `ioeq` vIsEOF x
                            True `ioeq` vIsOpen x
                            'a' `ioeq` vGetChar x
                            "bcd" `ioeq` vGetContents x
                            vClose x
               )
        ,f "line1\nline2\n\n\nline5\nlastline"
           (\x -> do False `ioeq` vIsEOF x
                     "line1" `ioeq` vGetLine x
                     "line2" `ioeq` vGetLine x
                     "" `ioeq` vGetLine x
                     "" `ioeq` vGetLine x
                     "line5" `ioeq` vGetLine x
                     "lastline" `ioeq` vGetLine x
                     assertRaises "eof error" (IOException $ mkIOError eofErrorType "" Nothing Nothing) (vGetLine x)
           )
        ]

tests = TestList [TestLabel "streamReader" (TestList test_StreamReader),
                  TestLabel "MemoryBuffer" (TestList test_MemoryBuffer)
                 ]