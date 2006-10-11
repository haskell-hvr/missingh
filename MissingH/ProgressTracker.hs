{-
Copyright (C) 2006 John Goerzen <jgoerzen@complete.org>

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
   Module     : MissingH.ProgressTracker
   Copyright  : Copyright (C) 2006 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable

Tools for tracking the status of a long operation.

Written by John Goerzen, jgoerzen\@complete.org

-}

module MissingH.ProgressTracker (-- * Types
                                 ProgressStatus(..),
                                 Progress, ProgressTimeSource,
                                 ProgressCallback,
                                 ProgressStatuses,
                                 -- * Creation and Options
                                 newProgress, newProgress',
                                 -- * Updating
                                 incrP, incrP', setP, incrTotal,
                                 setTotal,
                                 -- * Reading and Processing
                                 getSpeed,
                                 withStatus,
                                 getETR,
                                 getETA,
                                 -- * Utilities
                                 defaultTimeSource
                               )

where
import Control.Concurrent.MVar
import System.Time
import MissingH.Time
import Data.Ratio

----------------------------------------------------------------------
-- TYPES
----------------------------------------------------------------------

type ProgressTimeSource = IO Integer
type ProgressCallback = ProgressRecord -> ProgressRecord -> IO ()

{- | The main progress status record. -}
data ProgressStatus = 
     ProgressStatus {completedUnits :: Integer,
                     totalUnits :: Integer,
                     startTime :: Integer,
                     trackerName :: String,
                     timeSource :: ProgressTimeSource
                    }

data ProgressRecord =
    ProgressRecord {parents :: [Progress],
                    callbacks :: [ProgressCallback],
                    status :: ProgressStatus}

newtype Progress = Progress (MVar ProgressRecord)

class ProgressStatuses a b where
    withStatus :: a -> (ProgressStatus -> b) -> b

class ProgressRecords a b where
    withRecord :: a -> (ProgressRecord -> b) -> b

{-
instance ProgressStatuses ProgressRecord b where
    withStatus x func = func (status x)
instance ProgressRecords ProgressRecord b where
    withRecord x func = func x
-}

instance ProgressStatuses Progress (IO b) where
    withStatus (Progress x) func = withMVar x (\y -> func (status y))
instance ProgressRecords Progress (IO b) where
    withRecord (Progress x) func = withMVar x func

instance ProgressStatuses ProgressStatus b where
    withStatus x func = func x

----------------------------------------------------------------------
-- Creation
----------------------------------------------------------------------

{- | Create a new 'Progress' object with the given name and number
of total units initialized as given.  The start time will be initialized
with the current time at the present moment according to the system clock.
The units completed will be set to 0, the time source will be set to the
system clock, and the parents and callbacks will be empty.

If you need more control, see 'newProgress\''.
-}
newProgress :: String           -- ^ Name of this tracker
            -> Integer          -- ^ Total units expected
            -> IO Progress
newProgress name total =
    do t <- defaultTimeSource
       newProgress' (ProgressStatus {completedUnits = 0, totalUnits = total,
                                     startTime = t, trackerName = name,
                                     timeSource = defaultTimeSource})
                    []

{- | Create a new 'Progress' object initialized with the given status and 
callbacks.
No adjustment to the 'startTime' will be made.  If you
want to use the system clock, you can initialize 'startTime' with
the return value of 'defaultTimeSource' and also pass 'defaultTimeSource'
as the timing source. -}
newProgress' :: ProgressStatus
             -> [ProgressCallback] -> IO Progress
newProgress' news newcb =
    do r <- newMVar $ ProgressRecord {parents = [],
                                      callbacks = newcb, status = news}
       return (Progress r)

----------------------------------------------------------------------
-- Updating
----------------------------------------------------------------------
{- | Increment the completed unit count in the 'Progress' object
by the amount given.  If the value as given exceeds the total, then
the total will also be raised to match this value so that the 
completed count never exceeds the total.

You can decrease the completed unit count by supplying a negative number
here. -}
incrP :: Progress -> Integer -> IO ()
incrP po count = modStatus po statusfunc
    where statusfunc s = 
             s {completedUnits = newcu s,
                totalUnits = if newcu s > totalUnits s
                                 then newcu s
                                 else totalUnits s}
          newcu s = completedUnits s + count                  

{- | Like 'incrP', but never modify the total. -}
incrP' :: Progress -> Integer -> IO ()
incrP' po count = 
    modStatus po (\s -> s {completedUnits = completedUnits s + count})

{- | Set the completed unit count in the 'Progress' object to the specified
value.  Unlike 'incrP', this function sets the count to a specific value,
rather than adding to the existing value.  If this value exceeds the total,
then the total will also be raised to match this value so that the completed
count never exceeds teh total. -}
setP :: Progress -> Integer -> IO ()
setP po count = modStatus po statusfunc
    where statusfunc s =
              s {completedUnits = count,
                 totalUnits = if count > totalUnits s
                                  then count
                                  else totalUnits s}

{- | Like 'setP', but never modify the total. -}
setP' :: Progress -> Integer -> IO ()
setP' po count = modStatus po (\s -> s {completedUnits = count})

{- | Increment the total unit count in the 'Progress' object by the amount
given.  This would rarely be needed, but could be needed in some special cases 
when the total number of units is not known in advance. -}
incrTotal :: Progress -> Integer -> IO ()
incrTotal po count = 
    modStatus po (\s -> s {totalUnits = totalUnits s + count})

{- | Set the total unit count in the 'Progress' object to the specified
value.  Like 'incrTotal', this would rarely be needed. -}
setTotal :: Progress -> Integer -> IO ()
setTotal po count =
    modStatus po (\s -> s {totalUnits = count})

----------------------------------------------------------------------
-- Reading and Processing
----------------------------------------------------------------------

{- | Returns the speed in units processed per time unit.  (If you are
using the default time source, this would be units processed per second).
This obtains the current speed solely from analyzing the 'Progress' object.

If no time has elapsed yet, returns 0.

You can use this against either a 'Progress' object or a 'ProgressStatus'
object.  This is in the IO monad because the speed is based on the current
time.

Example:
>getSpeed progressobj >>= print
-}
getSpeed :: (ProgressStatuses a (IO b), Fractional b) => a -> IO b
getSpeed po = withStatus po $ \status -> 
                do t <- timeSource status
                   let elapsed = t - (startTime status)
                   return $ if elapsed == 0
                       then fromRational 0
                       else fromRational ((completedUnits status) % elapsed)

{- | Returns the estimated time remaining, in standard time units. -}
getETR :: (ProgressStatuses a (IO Integer),
           ProgressStatuses a (IO Rational)) => a -> IO Integer
getETR po = 
    do speed <- ((getSpeed po)::IO Rational)
       -- FIXME: potential for a race condition here, but it should
       -- be negligible
       withStatus po $ \status ->
           do let remaining = totalUnits status - completedUnits status
              return $ round $ (toRational remaining) / speed

{- | Returns the estimated system clock time of completion, in standard
time units. -}
getETA :: (ProgressStatuses a (IO Integer),
           ProgressStatuses a (IO Rational)) => a -> IO Integer
getETA po =
    do etr <- getETR po
       -- FIXME: similar race potential here
       withStatus po $ \status ->
           do timenow <- timeSource status
              return $ timenow + etr

----------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------
{- | The default time source for the system.  This is defined as:

>getClockTime >>= (return . clockTimeToEpoch)
-}
defaultTimeSource :: ProgressTimeSource
defaultTimeSource = getClockTime >>= (return . clockTimeToEpoch)

now :: ProgressRecords a ProgressTimeSource => a -> ProgressTimeSource
now x = withRecord x (timeSource . status)

modStatus :: Progress -> (ProgressStatus -> ProgressStatus) -> IO ()
-- FIXME/TODO: handle parents
modStatus (Progress mp) func =
    modifyMVar_ mp modfunc
    where modfunc :: ProgressRecord -> IO ProgressRecord
          modfunc pr = 
              do let newpr = pr {status = func (status pr)}
                 mapM_ (\x -> x pr newpr) (callbacks pr)
                 return newpr
