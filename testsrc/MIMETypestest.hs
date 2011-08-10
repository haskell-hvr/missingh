{- arch-tag: MIMETypes tests main file
Copyright (C) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

module MIMETypestest(tests) where
import Test.HUnit
import Data.List
import Data.MIME.Types

test_readMIMETypes =
    let omtd = readMIMETypes defaultmtd True "testsrc/mime.types.test"
        f = \strict inp exp -> TestCase $ do 
                                          mtd <- omtd
                                          exp @=? guessType mtd strict inp
        fe = \strict inp exp -> TestCase $ do mtd <- omtd
                                              (sort exp) @=? sort (guessAllExtensions mtd strict inp)
        in [
            f True "foo.bar.baz" (Nothing, Nothing)
           ,f True "" (Nothing, Nothing)
           ,f True "foo.ez" (Just "application/andrew-inset", Nothing)
           ,fe True "application/andrew-inset" [".ez"]
           ,f True "foo.dv" (Just "video/x-dv", Nothing)
           ,fe True "video/x-dv" [".dif", ".dv"]
           ,f True "test.h++" (Just "text/x-c++hdr", Nothing)
           ,fe True "text/x-c++hdr" [".h++", ".hpp", ".hxx", ".hh"]
           ,f True "foo.tgz" (Just "application/x-tar", Just "gzip")
           ]


test_guessAllExtensions =
    let f strict inp exp = TestCase $ (sort exp) @=? sort (guessAllExtensions defaultmtd strict inp) in
        [
         f True "" []
        ,f True "foo" []
        ,f True "application/octet-stream" [".obj", ".so", ".bin", ".a", ".dll", ".exe", ".o"]
        ,f True "text/plain" [".pl", ".ksh", ".bat", ".c", ".h", ".txt"]
        ,f True "application/rtf" []
        ,f False "application/rtf" [".rtf"]
        ]

test_guessType =
    let f strict inp exp = TestCase $ exp @=? guessType defaultmtd strict inp in 
         [
            f True "" (Nothing, Nothing)
           ,f True "foo" (Nothing, Nothing)
           ,f True "foo.txt" (Just "text/plain", Nothing)
           ,f True "foo.txt.gz" (Just "text/plain", Just "gzip")
           ,f True "foo.txt.blah" (Nothing, Nothing)
           ,f True "foo.tar" (Just "application/x-tar", Nothing)
           ,f True "foo.tar.gz" (Just "application/x-tar", Just "gzip")
           ,f True "foo.tgz" (Just "application/x-tar", Just "gzip")
           ,f True "http://foo/test.dir/blah.rtf" (Nothing, Nothing)
           ,f False "http://foo/test.dir/blah.rtf" (Just "application/rtf", Nothing)
           ,f True "foo.pict" (Nothing, Nothing)
           ,f False "foo.pict" (Just "image/pict", Nothing)
           ]

tests = TestList [TestLabel "guessType" (TestList test_guessType),
                  TestLabel "guessAllExtensions" (TestList test_guessAllExtensions),
                  TestLabel "readMIMETypes" (TestList test_readMIMETypes)
                 ]
