{- arch-tag: FiniteMap tests main file
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

module FiniteMaptest(tests) where
import HUnit
import MissingH.FiniteMap
import Data.FiniteMap

instance (Show a, Show b) => Show (FiniteMap a b) where
    show fm = show (fmToList fm)

test_flipFM =
    let f inp exp = (listToFM exp) @=? flipFM (listToFM inp) in
        do
        f ([]::[(Int,Int)]) ([]::[(Int,[Int])])
        f [("a", "b")] [("b", ["a"])]
        f [("a", "b"),
           ("c", "b"),
           ("d", "e"),
           ("b", "b")] [("b", ["c", "b", "a"]),
                        ("e", ["d"])]

tests = TestList [TestLabel "flipFM" (TestCase test_flipFM)
                 ]