{- arch-tag: Time utilities main file
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

{- |
   Module     : MissingH.Time
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

This module provides various helpful utilities for dealing with times and
dates.

Written by John Goerzen, jgoerzen\@complete.org
-}

module MissingH.Time(
                     calendarTimeUTCToEpoch,
                     calendarTimeToEpoch,
                     timeDiffToSecs
                    )
where
import System.Time

{- | January 1, 1970 represented as a CalendarTime. -}
epoch :: CalendarTime
epoch = CalendarTime { ctYear = 1970, ctMonth = January,
                       ctDay = 1, ctHour = 0, ctMin = 0, ctSec = 0,
                       ctPicosec = 0, ctWDay = Thursday, ctYDay = 0,
                       ctTZName = "", ctTZ = 0, ctIsDST = False}

{- | Converts the specified CalendarTime (see System.Time) to seconds-since-epoch time.

The conversion is naive with respect to timezones.  All timezone and DST information
in the CalendarTime object is silently ignored.  The epoch is assumed to be
00:00 on January 1, 1970.

One convenient side-effect of this zone-agnostic approach is that you will
get proper results out of this function whether or not you pass it UTC data,
since effectively it is computing a difference. -}

calendarTimeUTCToEpoch :: CalendarTime -> Integer
calendarTimeUTCToEpoch ct =
    timeDiffToSecs (diffClockTimes (toClockTime ct) (toClockTime epoch))

{- | Like 'calendarTimeUTCToEpoch', but works on local times.

Caveat: may be inaccurate right around the change to/from DST.

Ignores same fields as 'calendarTimeUTCToEpoch'.
-}

calendarTimeToEpoch :: CalendarTime -> IO Integer
calendarTimeToEpoch ct =
    do guessct <- toCalendarTime guesscl
       let newct = ct {ctTZ = ctTZ guessct}
       return $ calendarTimeUTCToEpoch newct
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
