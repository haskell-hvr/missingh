{- arch-tag: Tests main file
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

module Tests(tests) where
import HUnit
import qualified MIMETypestest
import qualified Listtest
import qualified FiniteMaptest
import qualified Pathtest
import qualified Strtest
import qualified IOtest
import qualified Bitstest
import qualified Network.FTP.Parsertest

test1 = TestCase ("x" @=? "x")

tests = TestList [TestLabel "test1" test1,
                 TestLabel "List" Listtest.tests,
                 TestLabel "Str" Strtest.tests,
                 TestLabel "FiniteMap" FiniteMaptest.tests,
                 TestLabel "Path" Pathtest.tests,
                 TestLabel "MIMETypes" MIMETypestest.tests,
                 TestLabel "Bitstest" Bitstest.tests,
                 TestLabel "Network.FTP.Parser" Network.FTP.Parsertest.tests]


