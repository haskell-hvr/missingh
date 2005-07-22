{- arch-tag: Time utilities main file
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

{- |
   Module     : MissingH.Time
   Copyright  : Copyright (C) 2004-2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

This module provides various Haskell utilities for dealing with times and
dates.

Written by John Goerzen, jgoerzen\@complete.org
-}

module MissingH.Time(
                     timelocal,
                     timegm,
                     timeDiffToSecs,
                     epoch,
                     epochToClockTime,
                     clockTimeToEpoch
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
