{- arch-tag: AnyDBM tests main file
Copyright (C) 2004-2005 John Goerzen <jgoerzen@complete.org>

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

module AnyDBMtest(mf, generic_test, tests) where
import HUnit
import MissingH.List
import MissingH.IO.HVFS
import MissingH.IO.HVFS.InstanceHelpers
import MissingH.AnyDBM
import MissingH.AnyDBM.StringDBM
import Data.HashTable
import Data.List(sort)
import Control.Exception(finally)

mf :: AnyDBM a => IO b -> (b -> IO a) -> String -> (a -> Assertion) -> Test
mf initfunc openfunc msg code =
    TestLabel msg $ TestCase $ do i <- initfunc
                                  h <- openfunc i
                                  finally (code h) (closeA h)
        
infix 1 @>=?
(@>=?) :: (Eq a, Show a) => a -> IO a -> Assertion
(@>=?) exp res = do r <- res
                    exp @=? r

deleteall h = do k <- keysA h
                 mapM_ (deleteA h) k
                 [] @>=? keysA h

weirdl = sort $ [("", "empty"), 
                 ("foo\nbar", "v1\0v2"),
                 ("v3,v4", ""),
                 ("k\0ey", "\xFF")]

generic_test initfunc openfunc =
    let f = mf initfunc openfunc in
        [
         f "empty" $ \h -> do [] @>=? keysA h
                              [] @>=? valuesA h
                              [] @>=? toListA h
                              Nothing @>=? lookupA h "foo"
                     
        ,f "basic" $ \h -> do insertA h "key" "value"
                              (Just "value") @>=? lookupA h "key"
                              [("key", "value")] @>=? toListA h
                              insertA h "key" "v2"
                              [("key", "v2")] @>=? toListA h
                              deleteA h "key"
                              [] @>=? toListA h
        ,f "mult" $ \h -> do insertListA h [("1", "2"), ("3", "4"), ("5", "6")]
                             [("1", "2"), ("3", "4"), ("5", "6")] @>=? 
                                (toListA h >>= return . sort)
                             ["1", "3", "5"] @>=? (keysA h >>= return . sort)
                             ["2", "4", "6"] @>=? (valuesA h >>= return . sort)
                             deleteall h
        ,f "weirdchars" $ \h -> do insertListA h weirdl
                                   weirdl @>=? (toListA h >>= return . sort)
                                   deleteall h
        ]

test_hashtable = generic_test (return ())
                  (\_ -> ((new (==) hashString)::IO (HashTable String String)))
test_stringdbm = generic_test (return SystemFS)
                   (\f -> openStringVDBM f "testsrc/tmp/StringDBM" ReadWriteMode)

tests = TestList [TestLabel "HashTable" (TestList test_hashtable),
                  TestLabel "StringDBM" (TestList test_stringdbm)
                 ]



