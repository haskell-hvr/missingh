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
import Test.HUnit
import qualified MIMETypestest
import qualified Listtest
import qualified Maptest
import qualified AnyDBMtest
import qualified Pathtest
import qualified Strtest
import qualified IOtest
import qualified Bitstest
import qualified Eithertest
import qualified ConfigParser.Parsertest
import qualified ConfigParser.Maintest
import qualified CRC32POSIXtest
import qualified CRC32GZIPtest
import qualified GZiptest
import qualified HVIOtest
import qualified HVFStest
import qualified Timetest
import qualified Str.CSVtest
import qualified WildMatchtest
import qualified Globtest
import qualified ProgressTrackertest

test1 = TestCase ("x" @=? "x")

tests = TestList [TestLabel "test1" test1,
                 TestLabel "List" Listtest.tests,
                 TestLabel "Str" Strtest.tests,
                 TestLabel "CSV" Str.CSVtest.tests,
                 TestLabel "Time" Timetest.tests,
                 TestLabel "Map" Maptest.tests,
                 TestLabel "ProgressTracker" ProgressTrackertest.tests,
                 TestLabel "AnyDBM" AnyDBMtest.tests,
                 TestLabel "Path" Pathtest.tests,
                 TestLabel "WildMatch" WildMatchtest.tests,
                 TestLabel "HVIO" HVIOtest.tests,
                 TestLabel "HVFS" HVFStest.tests,
                 TestLabel "Glob" Globtest.tests,
                 TestLabel "MIMETypes" MIMETypestest.tests,
                 TestLabel "Bitstest" Bitstest.tests,
                 TestLabel "Eithertest" Eithertest.tests,
                 TestLabel "ConfigParser.RunParser" ConfigParser.Parsertest.tests,
                 TestLabel "ConfigParser.Main" ConfigParser.Maintest.tests,
                 TestLabel "CRC32POSIXtest" CRC32POSIXtest.tests,
                 TestLabel "CRC32GZIPtest" CRC32GZIPtest.tests,
                 TestLabel "GZiptest" GZiptest.tests]


