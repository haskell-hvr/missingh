{- arch-tag: Tests main file
Copyright (C) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

module Tests(tests) where
import Test.HUnit
import qualified MIMETypestest
import qualified Listtest
import qualified Maptest
import qualified Pathtest
import qualified Strtest
import qualified IOtest
import qualified Bitstest
import qualified Eithertest
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
                 TestLabel "Path" Pathtest.tests,
                 TestLabel "WildMatch" WildMatchtest.tests,
                 TestLabel "HVIO" HVIOtest.tests,
                 TestLabel "HVFS" HVFStest.tests,
                 TestLabel "Glob" Globtest.tests,
                 TestLabel "MIMETypes" MIMETypestest.tests,
                 TestLabel "Bitstest" Bitstest.tests,
                 TestLabel "Eithertest" Eithertest.tests,
                 TestLabel "CRC32POSIXtest" CRC32POSIXtest.tests,
                 TestLabel "CRC32GZIPtest" CRC32GZIPtest.tests,
                 TestLabel "GZiptest" GZiptest.tests]


