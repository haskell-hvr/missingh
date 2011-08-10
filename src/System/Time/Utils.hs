{- arch-tag: Time utilities main file
Copyright (c) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : System.Time.Utils
   Copyright  : Copyright (C) 2004-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable

This module provides various Haskell utilities for dealing with times and
dates.

Written by John Goerzen, jgoerzen\@complete.org
-}

module System.Time.Utils(
                     timelocal,
                     timegm,
                     timeDiffToSecs,
                     epoch,
                     epochToClockTime,
                     clockTimeToEpoch,
                     renderSecs, renderTD
                    )
where
import System.Time
import Data.Ratio

{- | January 1, 1970, midnight, UTC, represented as a CalendarTime. -}
epoch :: CalendarTime
epoch = CalendarTime { ctYear = 1970, ctMonth = January,
                       ctDay = 1, ctHour = 0, ctMin = 0, ctSec = 0,
                       ctPicosec = 0, ctWDay = Thursday, ctYDay = 0,
                       ctTZName = "UTC", ctTZ = 0, ctIsDST = False}

{- | Converts the specified CalendarTime (see System.Time) to seconds-since-epoch time.

This conversion does respect the timezone specified on the input object.
If you want a conversion from UTC, specify ctTZ = 0 and ctIsDST = False.

When called like that, the behavior is equivolent to the GNU C function
timegm().  Unlike the C library, Haskell's CalendarTime supports
timezone information, so if such information is specified, it will impact
the result.
-}

timegm :: CalendarTime -> Integer
timegm ct =
    timeDiffToSecs (diffClockTimes (toClockTime ct) (toClockTime epoch))

{- | Converts the specified CalendarTime (see System.Time) to 
seconds-since-epoch format.

The input CalendarTime is assumed to be the time as given in your local
timezone.  All timezone and DST fields in the object are ignored.

This behavior is equivolent to the timelocal() and mktime() functions that
C programmers are accustomed to.

Please note that the behavior for this function during the hour immediately
before or after a DST switchover may produce a result with a different hour
than you expect.
-}

timelocal :: CalendarTime -> IO Integer
timelocal ct =
    do guessct <- toCalendarTime guesscl
       let newct = ct {ctTZ = ctTZ guessct}
       return $ timegm newct
    where guesscl = toClockTime ct
    
{- | Converts the given timeDiff to the number of seconds it represents. 

Uses the same algorithm as normalizeTimeDiff in GHC. -}
timeDiffToSecs :: TimeDiff -> Integer
timeDiffToSecs td = 
    (fromIntegral $ tdSec td) +
    60 * ((fromIntegral $ tdMin td) +
          60 * ((fromIntegral $ tdHour td) +
                24 * ((fromIntegral $ tdDay td) +
                      30 * ((fromIntegral $ tdMonth td) +
                            365 * (fromIntegral $ tdYear td)))))

{- | Converts an Epoch time represented with an arbitrary Real to a ClockTime.
This input could be a CTime from Foreign.C.Types or an EpochTime from
System.Posix.Types. -}
epochToClockTime :: Real a => a -> ClockTime
epochToClockTime x =
    TOD seconds secfrac
    where ratval = toRational x
          seconds = floor ratval
          secfrac = floor $ (ratval - (seconds % 1) ) * picosecondfactor
          picosecondfactor = 10 ^ 12
          
{- | Converts a ClockTime to something represented with an arbitrary Real.
The result could be treated as a CTime from Foreign.C.Types or EpochTime from
System.Posix.Types.  The inverse of 'epochToClockTime'.

Fractions of a second are not preserved by this function. -}
clockTimeToEpoch :: Num a => ClockTime -> a
clockTimeToEpoch (TOD sec _) = fromInteger sec

{- | Render a number of seconds as a human-readable amount.  Shows the two
most significant places.  For instance:

>renderSecs 121 = "2m1s"

See also 'renderTD' for a function that works on a TimeDiff.
-}
renderSecs :: Integer -> String
renderSecs i = renderTD $ diffClockTimes (TOD i 0) (TOD 0 0)

{- | Like 'renderSecs', but takes a TimeDiff instead of an integer second
count. -}
renderTD :: TimeDiff -> String
renderTD itd =
    case workinglist of
      [] -> "0s"
      _ -> concat . map (\(q, s) -> show q ++ [s]) $ workinglist
    where td = normalizeTimeDiff itd
          suffixlist = "yMdhms"
          quantlist = (\(TimeDiff y mo d h m s _) -> [y, mo, d, h, m, s]) td
          zippedlist = zip quantlist suffixlist
          -- Drop all leading elements that are 0, then take at most 2
          workinglist = take 2 . dropWhile (\(q, _) -> q == 0) $ zippedlist

