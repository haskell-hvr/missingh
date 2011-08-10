{- arch-tag: Tests for GZip module
Copyright (C) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

module GZiptest(tests) where
import Test.HUnit
import System.FileArchive.GZip
import System.FilePath
import Data.Compression.Inflate
import System.IO.Binary
import System.IO
import Data.Either.Utils
import Data.List

mf fn exp conf = TestLabel fn $ TestCase $
                     do c <- readBinaryFile $
                          joinPath ["testsrc", "gzfiles", fn]
                        assertEqual "" exp (conf c)

{-
import System.FileArchive.GZip
import System.IO
import Data.Either.Utils

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
        --,f "zeros.gz" 10485760 (length . inflate_string)
        -- BAD BAD ,f "zeros.gz" (replicate (10 * 1048576) '\0') inflate_string
        -- This line tests Igloo's code:
        --,f "zeros.gz" True (\x -> (replicate 10485760 '\0') == inflate_string x)
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
    let f fn exp = mf fn exp decompress
        in
        [
         f "t1.gz" ("Test 1", Nothing)
        ,f "t1bad.gz" ("Test 1", Just CRCError)
        ,f "t2.gz" ("Test 1Test 2", Nothing)
        -- The following tests my code
         {-
        ,mf "zeros.gz" True (\x -> case decompress x of
                             (y, _) -> y == replicate 10485760 '\0'
                            )
         -}
        ]

tests = TestList [TestLabel "inflate" (TestList test_inflate),
                  TestLabel "header" (TestList test_header),
--                  TestLabel "bunches" (TestList test_bunches),
                  TestLabel "gunzip" (TestList test_gunzip)

                 ]

