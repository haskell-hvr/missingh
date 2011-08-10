{- arch-tag: HVFS tests main file
Copyright (C) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

module HVFStest(tests) where
import Test.HUnit
import System.IO.HVIO
import System.IO.HVFS
import System.IO.HVFS.InstanceHelpers
import System.IO.HVFS.Combinators
import Test.HUnit.Tools
import System.IO
import System.IO.Error
import Control.Exception

ioeq :: (Show a, Eq a) => a -> IO a -> Assertion
ioeq exp inp = do x <- inp
                  exp @=? x

testTree = [("test.txt", MemoryFile "line1\nline2\n"),
            ("file2.txt", MemoryFile "line3\nline4\n"),
            ("emptydir", MemoryDirectory []),
            ("dir1", MemoryDirectory
             [("file3.txt", MemoryFile "line5\n"),
              ("test.txt", MemoryFile "subdir test"),
              ("dir2", MemoryDirectory [])
             ]
            )
           ]

test_nice_slice =
    let f exp fp = TestLabel fp $ TestCase $ exp @=? nice_slice fp
        in [
            f [] "/"
           ,f ["foo", "bar"] "/foo/bar"
           --,f [] "."
           ]

test_content = 
    let f exp fp = TestLabel fp $ TestCase $
                     do x <- newMemoryVFS testTree
                        h <- vOpen x fp ReadMode
                        case h of
                           HVFSOpenEncap h2 -> exp `ioeq` vGetContents h2
        in
        [
         f "line1\nline2\n" "test.txt",
         f "line1\nline2\n" "/test.txt",
         f "line5\n" "dir1/file3.txt",
         f "subdir test" "/dir1/test.txt"
        ]

test_chroot =
    let f msg testfunc = TestLabel msg $ TestCase $ 
                         do x <- newMemoryVFS testTree
                            vSetCurrentDirectory x "/emptydir"
                            y <- newHVFSChroot x "/dir1"
                            testfunc y
        in
        [
         f "root" (\x -> ["file3.txt", "test.txt", "dir2"]
                   `ioeq` vGetDirectoryContents x "/")
        ,f "cwd" (\x -> "/" `ioeq` vGetCurrentDirectory x)
        ,f "dir2" (\x -> [] `ioeq` vGetDirectoryContents x "/dir2")
        ,f "dot" (\x -> ["file3.txt", "test.txt", "dir2"]
                  `ioeq` vGetDirectoryContents x ".")
        ,f "cwd tests" $
          (\x -> do a <- vGetDirectoryContents x "/"
                    ["file3.txt", "test.txt", "dir2"] @=? a
                    vSetCurrentDirectory x "/dir2"
                    cwd <- vGetCurrentDirectory x
                    "/dir2" @=? cwd
                    y <- vGetDirectoryContents x "."
                    [] @=? y
                    vSetCurrentDirectory x ".."
                    "/" `ioeq` vGetCurrentDirectory x
                    --vSetCurrentDirectory x ".."
                    --"/" `ioeq` vGetCurrentDirectory x
          )
        --,f "test.txt" (\x -> "subdir test" `ioeq` 
        --               (vOpen x "/test.txt" ReadMode >>= vGetContents))
        ]


test_structure =
    let f msg testfunc = TestLabel msg $ TestCase $ do x <- newMemoryVFS testTree
                                                       testfunc x
        in
        [
         f "root" (\x -> ["test.txt", "file2.txt", "emptydir", "dir1"]
                         `ioeq` vGetDirectoryContents x "/")
        ,f "dot" (\x -> ["test.txt", "file2.txt", "emptydir", "dir1"]
                  `ioeq` vGetDirectoryContents x ".")
        ,f "dot2" (\x -> ["file3.txt", "test.txt", "dir2"]
                   `ioeq` do vSetCurrentDirectory x "./dir1"
                             vGetDirectoryContents x ".")
        ,f "emptydir" (\x -> [] `ioeq` vGetDirectoryContents x "/emptydir")
        ,f "dir1" (\x -> ["file3.txt", "test.txt", "dir2"] `ioeq`
                   vGetDirectoryContents x "/dir1")
        ,f "dir1/dir2" (\x -> [] `ioeq` vGetDirectoryContents x "/dir1/dir2")
        ,f "relative tests" (\x -> 
            do vSetCurrentDirectory x "dir1"
               [] `ioeq` vGetDirectoryContents x "dir2"
                            )
        ]
                            

tests = TestList [TestLabel "nice_slice" (TestList test_nice_slice)
                 ,TestLabel "structure" (TestList test_structure)
                 ,TestLabel "content" (TestList test_content)
                 ,TestLabel "chroot" (TestList test_chroot)
                 ]
