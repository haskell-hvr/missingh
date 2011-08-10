{- arch-tag: Time tests main file
Copyright (C) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE

-}

module Timetest(tests) where
import Test.HUnit
import System.Time.Utils
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
