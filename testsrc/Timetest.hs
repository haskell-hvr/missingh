{- arch-tag: Time tests main file
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

module Timetest(tests) where
import HUnit
import MissingH.Time
import System.Time

base =CalendarTime {ctYear = 2005, ctMonth = January, ctDay = 21,
                          ctHour = 1, ctMin = 1, ctSec = 20,
                          ctPicosec = 0, ctWDay = Sunday, ctYDay = 0,
                          ctTZName = "", ctTZ = 0, ctIsDST = False}
test_ctu2e =
    let f base exp = TestLabel (show base) $ TestCase $ exp @=? timegm base in
        [
         f (base {ctYear = 2005, ctMonth = January, ctDay = 21,
                          ctHour = 1, ctMin = 1, ctSec = 20})
           1106269280
           
         ,f (base {ctYear = 2004, ctMonth = July, ctDay = 1,
                           ctHour = 17, ctMin = 0, ctSec = 0})
           1088701200

        ]

test_ct2e =
    let f base exp = TestLabel (show base) $ TestCase $ 
                       do r <- timelocal base
                          exp @=? r in
        [
         f (base {ctYear = 2005, ctMonth = January, ctDay = 20,
                          ctHour = 19, ctMin = 1, ctSec = 20})
           1106269280
        ,f (base {ctYear = 2004, ctMonth = July, ctDay = 1,
                           ctHour = 12, ctMin = 0, ctSec = 0})
           1088701200
        ]

tests = TestList [TestLabel "ctu2e" (TestList test_ctu2e)]
