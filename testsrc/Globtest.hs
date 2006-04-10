{- 
Copyright (C) 2006 John Goerzen <jgoerzen@complete.org>

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


module Globtest(tests) where
import Test.HUnit
import MissingH.Path.Glob
import MissingH.Path
import MissingH.HUnit
import MissingH.IO.HVFS
import System.Posix.Directory
import System.Posix.Files
import Control.Exception
import Data.List

bp = "testtmp"
touch x = writeFile x ""

globtest thetest = 
    bracket_ (setupfs)
             (recursiveRemove SystemFS bp)
             thetest
    where setupfs =
              do mapM_ (\x -> createDirectory x 0o755)
                       [bp, bp ++ "/a", bp ++ "/aab", bp ++ "/aaa",
                        bp ++ "/ZZZ", bp ++ "/a/bcd",
                        bp ++ "/a/bcd/efg"]
                 mapM_ touch [bp ++ "/a/D", bp ++ "/aab/F", bp ++ "/aaa/zzzF",
                              bp ++ "/a/bcd/EF", bp ++ "/a/bcd/efg/ha"]
                 createSymbolicLink "broken" "sym1"
                 createSymbolicLink "broken" "sym2"
                 
assertEqualSorted msg exp res =
    assertEqual msg (sort exp) (sort res)
eq = assertEqualSorted
mf msg func = TestLabel msg $ TestCase $ globtest func
f func = TestCase $ globtest func
preppath x = bp ++ "/" ++ x

test_literal =
    map f
            [glob (preppath "a") >>= eq "" [(preppath "a")]
            ,glob (preppath "a/D") >>= eq "" [(preppath "a/D")]
            ,glob (preppath "aab") >>= eq "" [(preppath "aab")]
            ,glob (preppath "nonexistant") >>= eq "empty" []
            ]

tests = TestList [TestLabel "test_literal" (TestList test_literal)]




