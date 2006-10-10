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
                                 ProgressStatuses(..),
                                 -- * Use
                                 newProgress,
                                 -- * Utilities
                                 defaultTimeSource
                               )

where
import Control.Concurrent.MVar
import System.Time
import MissingH.Time

----------------------------------------------------------------------
-- TYPES
----------------------------------------------------------------------

type ProgressTimeSource = IO Integer
type ProgressCallback = ProgressRecord -> IO ()

{- | The main progress status record. -}
data ProgressStatus = 
     ProgressStatus {completedUnits :: Integer,
                     totalUnits :: Integer,
                     startTime :: Integer,
                     trackerName :: String}
     deriving (Eq, Show, Read)

data ProgressRecord =
    ProgressRecord {timeSource :: ProgressTimeSource,
                    parents :: [Progress],
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
-- Use
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
                                     startTime = t, trackerName = name})
                    defaultTimeSource []

{- | Create a new 'Progress' object initialized with the given status,
time source, and callbacks.
No adjustment to the 'startTime' will be made.  If you
want to use the system clock, you can initialize 'startTime' with
the return value of 'defaultTimeSource' and also pass 'defaultTimeSource'
as the timing source. -}
newProgress' :: ProgressStatus -> ProgressTimeSource
             -> [ProgressCallback] -> IO Progress
newProgress' news newts newcb =
    do r <- newMVar $ ProgressRecord {timeSource = newts, parents = [],
                                      callbacks = newcb, status = news}
       return (Progress r)


----------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------
{- | The default time source for the system.  This is defined as:

>getClockTime >>= (return . clockTimeToEpoch)
-}
defaultTimeSource :: ProgressTimeSource
defaultTimeSource = getClockTime >>= (return . clockTimeToEpoch)

now :: ProgressRecords a ProgressTimeSource => a -> ProgressTimeSource
now x = withRecord x timeSource

