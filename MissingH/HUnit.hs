{- arch-tag: Test utilities
Copyright (C) 2004 - 2005 John Goerzen <jgoerzen@complete.org>

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

{- |
   Module     : MissingH.HUnit
   Copyright  : Copyright (C) 2004-2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Utilities for HUnit unit testing.

Written by John Goerzen, jgoerzen\@complete.org
-}



module MissingH.HUnit(assertRaises, mapassertEqual, qccheck, qctest) where
import Test.HUnit
import Test.QuickCheck as QC
import qualified Control.Exception
import System.Random

{- | Asserts that a specific exception is raised by a given action. -}

assertRaises :: Show a => String -> Control.Exception.Exception -> IO a -> IO ()
assertRaises msg selector action =
    let thetest e = if e == selector then return ()
                    else assertFailure $ msg ++ "\nReceived unexpected exception: "
                             ++ (show e) ++ "\ninstead of exception: " ++ (show selector)
        in do
           r <- Control.Exception.try action
           case r of
                  Left e -> thetest e
                  Right x -> assertFailure $ msg ++ "\nReceived no exception, but was expecting exception: " ++ (show selector)

mapassertEqual :: (Show b, Eq b) => String -> (a -> b) -> [(a, b)] -> [Test]
mapassertEqual descrip func [] = []
mapassertEqual descrip func ((inp,result):xs) =
    (TestCase $ assertEqual descrip result (func inp)) : mapassertEqual descrip func xs

-- * Turn QuickCheck tests into HUnit tests

-- |qccheck turns the quickcheck test into an hunit test
qccheck :: (QC.Testable a) => 
           Config -- ^ quickcheck config
        -> String -- ^ label for the property
        -> a      -- ^ quickcheck property
        -> Test
qccheck config lbl property =
    TestLabel lbl $ TestCase $
              do rnd <- newStdGen
                 tests config (evaluate property) rnd 0 0 []

-- |qctest is equivalent to 'qccheck defaultConfig'
qctest ::  (QC.Testable a) => String -> a -> Test
qctest lbl property = qccheck defaultConfig lbl property

-- |modified version of the tests function from Test.QuickCheck
tests :: Config -> Gen Result -> StdGen -> Int -> Int -> [[String]] -> IO () 
tests config gen rnd0 ntest nfail stamps
  | ntest == configMaxTest config = return () 
  | nfail == configMaxFail config = assertFailure $ "Arguments exhausted after " ++ show ntest ++ " tests."
  | otherwise               =
      do putStr (configEvery config ntest (arguments result))
         case ok result of
           Nothing    ->
             tests config gen rnd1 ntest (nfail+1) stamps
           Just True  ->
             tests config gen rnd1 (ntest+1) nfail (stamp result:stamps)
           Just False ->
             assertFailure $  ( "Falsifiable, after "
                   ++ show ntest
                   ++ " tests:\n"
                   ++ unlines (arguments result)
                    )
     where
      result      = generate (configSize config ntest) rnd2 gen
      (rnd1,rnd2) = split rnd0
