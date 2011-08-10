{- arch-tag: HVIO tests main file
Copyright (C) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

module HVIOtest(tests) where
import Test.HUnit
import System.IO.HVIO
import Test.HUnit.Tools
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
                        assertRaises "eof error" (mkIOError eofErrorType "" Nothing Nothing) (vGetChar x)
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
                        vSeek x AbsoluteSeek 1
                        vPutStr x "IN"
                        vRewind x
                        "LINe1" `ioeq` vGetLine x
                        "Line2" `ioeq` vGetLine x                        
                        vSeek x SeekFromEnd 0
                        vPutChar x 'c'
                        assertRaises "eof error" (mkIOError eofErrorType "" Nothing Nothing) (vGetLine x)
                        vRewind x
                        "LINe1\nLine2\nc" `ioeq` vGetContents x
              )
        ]

test_StreamReader =
    let f inp testfunc = TestLabel inp $ TestCase $ do x <- newStreamReader inp
                                                       testfunc x
        in 
        [
         f "" (\x -> do True `ioeq` vIsEOF x
                        True `ioeq` vIsOpen x
                        assertRaises "eof error" (mkIOError eofErrorType "" Nothing Nothing) (vGetChar x)
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
                     assertRaises "eof error" (mkIOError eofErrorType "" Nothing Nothing) (vGetLine x)
           )
        ]

tests = TestList [TestLabel "streamReader" (TestList test_StreamReader),
                  TestLabel "MemoryBuffer" (TestList test_MemoryBuffer)
                 ]
