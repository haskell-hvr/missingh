{- arch-tag: MissingH.Either tests
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

module Eithertest(tests) where
import HUnit
import MissingH.Either
import Testutil
import Control.Exception

test_maybeToEither =
    let f msg inp exp = TestLabel msg $ TestCase $ assertEqual "" exp inp in
        [
         f "Nothing" (maybeToEither "error" (Nothing::Maybe String))
           (Left "error"),
         f "Nothing diff types" (maybeToEither "error" (Nothing::Maybe Int))
           (Left "error"),
         f "Just" (maybeToEither "error" (Just "good")) (Right "good"),
         f "Diff types" (maybeToEither "error" (Just (5::Int))) 
           (Right (5::Int))
        ]

test_forceEither =
    let f msg inp exp = TestLabel msg $ TestCase $ assertEqual "" exp inp in
    [
     f "Right" (forceEither ((Right "foo")::Either Int String)) "foo",
     TestLabel "Left" $ TestCase $ assertRaises "" (ErrorCall "\"wrong\"")
           ("" @=? forceEither (Left "wrong"))
    ]

tests = TestList [TestLabel "test_maybeToEither" (TestList test_maybeToEither),
                  TestLabel "test_forceEither" (TestList test_forceEither)
                 ]

