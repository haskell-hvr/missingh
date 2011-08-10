{- 
Copyright (C) 2006-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE

-}


module Globtest(tests) where
import Test.HUnit
import System.Path.Glob
import System.Path
import Test.HUnit.Tools
import System.IO.HVFS
import System.Directory(createDirectory)
#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
import System.Posix.Directory hiding (createDirectory)
import System.Posix.Files
#endif
import Control.Exception
import Data.List

bp = "testtmp"
touch x = writeFile x ""

globtest thetest = 
    bracket_ (setupfs)
             (recursiveRemove SystemFS bp)
             thetest
    where setupfs =
              do mapM_ (\x -> createDirectory x)
                       [bp, bp ++ "/a", bp ++ "/aab", bp ++ "/aaa",
                        bp ++ "/ZZZ", bp ++ "/a/bcd",
                        bp ++ "/a/bcd/efg"]
                 mapM_ touch [bp ++ "/a/D", bp ++ "/aab/F", bp ++ "/aaa/zzzF",
                              bp ++ "/a/bcd/EF", bp ++ "/a/bcd/efg/ha",
                             bp ++ "/a/foo", bp ++ "/a/afoo",
                             bp ++ "/a/a-foo", bp ++ "/a/a.foo"]
#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
                 createSymbolicLink (preppath "broken") (preppath "sym1")
                 createSymbolicLink (preppath "broken") (preppath "sym2")
#endif
                 
eq msg exp res =
    assertEqual msg (sort exp) (sort res)
mf msg func = TestLabel msg $ TestCase $ globtest func
f func = TestCase $ globtest func
preppath x = bp ++ "/" ++ x

test_literal =
    map f
            [glob (preppath "a") >>= eq "" [preppath "a"]
            ,glob (preppath "a/D") >>= eq "" [preppath "a/D"]
            ,glob (preppath "aab") >>= eq "" [preppath "aab"]
            ,glob (preppath "nonexistant") >>= eq "empty" []
            ]

test_one_dir =
    map f
        [glob (preppath "a*") >>= eq "a*" (map preppath ["a", "aab", "aaa"]),
         glob (preppath "*a") >>= eq "*a" (map preppath ["a", "aaa"]),
         glob (preppath "aa?") >>= eq "aa?" (map preppath ["aaa", "aab"]),
         glob (preppath "aa[ab]") >>= eq "aa[ab]" (map preppath ["aaa", "aab"]),
         glob (preppath "*q") >>= eq "*q" []
        ]

test_nested_dir =
    map f
        [glob (preppath "a/bcd/E*") >>= eq "a/bcd/E*" [preppath "a/bcd/EF"],
         glob (preppath "a/bcd/*g") >>= eq "a/bcd/*g" [preppath "a/bcd/efg"],
         glob (preppath "a/*.foo") >>= eq "a/*.foo" [preppath "a/a.foo"]
        ]

test_dirnames = 
    map f
        [glob (preppath "*/D") >>= eq "*/D" [preppath "a/D"],
         glob (preppath "*/*a") >>= eq "*/*a" [],
         glob (preppath "a/*/*/*a") >>= eq "a/*/*/*a" [preppath "a/bcd/efg/ha"],
         glob (preppath "?a?/*F") >>= eq "?a?/*F" (map preppath ["aaa/zzzF", "aab/F"])
        ]

test_brokensymlinks =
#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
    map f
        [glob (preppath "sym*") >>= eq "sym*" (map preppath ["sym1", "sym2"]),
         glob (preppath "sym1") >>= eq "sym1" [preppath "sym1"],
         glob (preppath "sym2") >>= eq "sym2" [preppath "sym2"]
        ]
#else
    []
#endif
         

tests = TestList [TestLabel "test_literal" (TestList test_literal),
                  TestLabel "test_one_dir" (TestList test_one_dir),
                  TestLabel "test_nested_dir" (TestList test_nested_dir),
                  TestLabel "test_dirnames" (TestList test_dirnames),
                  TestLabel "test_brokensymlinks" (TestList test_brokensymlinks)]




