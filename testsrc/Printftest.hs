{- arch-tag: Printf tests main file
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

module Printftest(tests) where
import HUnit
import MissingH.Printf

test_vsprintf = 
    do
    "" @=? vsprintf ""
    "%" @=? vsprintf "%%"
    "asdf" @=? vsprintf "%s" "asdf"
    "foo: 5" @=? vsprintf "%s: %d" "foo" (5::Integer)
    "%foo%:% %-1%\n%" @=? vsprintf "%%%s%%:%% %%%d%%\n%%" "foo" (-1::Integer)
    "baz: 3.140000" @=? vsprintf "%s: %f" "baz" (3.14::Double)
    "quux: 3.140000e+02" @=? vsprintf "%s: %e" "quux" (314::Double)
    "fe" @=? vsprintf "%x" (254::Integer)
    "FE" @=? vsprintf "%X" (254::Integer)
    "10" @=? vsprintf "%o" (8::Integer)

tests = TestList [TestLabel "vsprintf" (TestCase test_vsprintf)
                 ]