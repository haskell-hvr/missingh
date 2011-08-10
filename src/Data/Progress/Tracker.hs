{-
Copyright (c) 2006-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : Data.Progress.Tracker
   Copyright  : Copyright (C) 2006-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable

Tools for tracking the status of a long operation.

Written by John Goerzen, jgoerzen\@complete.org 

See also "Data.Progress.Meter" -}

module Data.Progress.Tracker (
                                 -- * Introduction
                                 -- $introduction
                                 -- ** Examples
                                 -- $examples
                                 -- * Creation and Options
                                 newProgress, newProgress',
                                 addCallback, addParent,
                                 -- * Updating
                                 incrP, incrP', setP, setP', incrTotal,
                                 setTotal, finishP,
                                 -- * Reading and Processing
                                 getSpeed,
                                 withStatus,
                                 getETR,
                                 getETA,
                                 -- * Types
                                 ProgressStatus(..),
                                 Progress, ProgressTimeSource,
                                 ProgressCallback,
                                 ProgressStatuses,
                                 -- * Utilities
                                 defaultTimeSource
                               )

where
import Control.Concurrent.MVar
import System.Time
import System.Time.Utils
import Data.Ratio

{- $introduction

ProgressTracker is a module for tracking the progress on long-running
operations.  It can be thought of as the back end engine behind
a status bar.  ProgressTracker can do things such as track how far along
a task is, provide an estimated time of completion, estimated time remaining,
current speed, etc.  It is designed to be as generic as possible; it can even
base its speed calculations on something other than the system clock.

ProgressTracker also supports a notion of a parent tracker.  This is used when
a large task is composed of several individual tasks which may also be
long-running.  Downloading many large files over the Internet is a common
example of this.

Any given ProgressTracker can be told about one or more parent trackers.  
When the child tracker's status is updated, the parent tracker's status is
also updated in the same manner.  Therefore, the progress on each individual
component, as well as the overall progress, can all be kept in sync
automatically.

Finally, you can register callbacks.  Callbacks are functions that are called
whenever the status of a tracker changes.  They'll be passed the old and new
status and are intended to do things like update on-screen status displays.

The cousin module 'Data.Progress.Meter' can be used to nicely render
these trackers on a console.
-}

{- $examples

Here is an example use:

>do prog <- newProgress "mytracker" 1024
>   incrP prog 10
>   getETR prog >>= print           -- prints number of seconds remaining
>   incrP prog 500
>   finishP prog
-}

----------------------------------------------------------------------
-- TYPES
----------------------------------------------------------------------

{- | A function that, when called, yields the current time. 
The default is 'defaultTimeSource'. -}
type ProgressTimeSource = IO Integer

{- | The type for a callback function for the progress tracker.
When given at creation time to 'newProgress\'' or when added via 'addCallback',
these functions get called every time the status of the tracker changes.

This function is passed two 'ProgressStatus' records: the first
reflects the status prior to the update, and the second reflects
the status after the update.

Please note that the owning 'Progress' object will be locked while the
callback is running, so the callback will not be able to make changes to it. -}
type ProgressCallback = ProgressStatus -> ProgressStatus -> IO ()

{- | The main progress status record. -}
data ProgressStatus = 
     ProgressStatus {completedUnits :: Integer,
                     totalUnits :: Integer,
                     startTime :: Integer,
                     trackerName :: String, -- ^ An identifying string
                     timeSource :: ProgressTimeSource
                    }

data ProgressRecord =
    ProgressRecord {parents :: [Progress],
                    callbacks :: [ProgressCallback],
                    status :: ProgressStatus}

{- | The main Progress object. -}
newtype Progress = Progress (MVar ProgressRecord)

class ProgressStatuses a b where
    {- | Lets you examine the 'ProgressStatus' that is contained 
       within a 'Progress' object.  You can simply pass
       a 'Progress' object and a function to 'withStatus', and
       'withStatus' will lock the 'Progress' object (blocking any
       modifications while you are reading it), then pass the object
       to your function.  If you happen to already have a 'ProgressStatus'
       object, withStatus will also accept it and simply pass it unmodified
       to the function. -}
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

Example:

> prog <- newProgress "mytracker" 1024

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

{- | Adds an new callback to an existing 'Progress'.  The callback will be
called whenever the object's status is updated, except by the call to finishP.

Please note that the Progress object will be locked while the callback is 
running, so the callback will not be able to make any modifications to it.
-}
addCallback :: Progress -> ProgressCallback -> IO ()
addCallback (Progress mpo) cb = modifyMVar_ mpo $ \po ->
    return $ po {callbacks = cb : callbacks po}

{- | Adds a new parent to an existing 'Progress'.  The parent
will automatically have its completed and total counters incremented
by the value of those counters in the existing 'Progress'. -}
addParent :: Progress           -- ^ The child object
          -> Progress           -- ^ The parent to add to this child
          -> IO ()
addParent (Progress mcpo) ppo = modifyMVar_ mcpo $ \cpo ->
    do incrP' ppo (completedUnits . status $ cpo)
       incrTotal ppo (totalUnits . status $ cpo)
       return $ cpo {parents = ppo : parents cpo}

{- | Call this when you are finished with the object.  It is especially
important to do this when parent objects are involved.

This will simply set the totalUnits to the current completedUnits count,
but will not call the callbacks.  It will additionally propogate
any adjustment in totalUnits to the parents, whose callbacks /will/ be
called.

This ensures that the total expected counts on the parent are always correct.
Without doing this, if, say, a transfer ended earlier than expected, ETA 
values on the parent would be off since it would be expecting more data than
actually arrived. -}
finishP :: Progress -> IO ()
finishP (Progress mp) =
    modifyMVar_ mp modfunc
    where modfunc :: ProgressRecord -> IO ProgressRecord
          modfunc oldpr =
              do let adjustment = (completedUnits . status $ oldpr) 
                                  - (totalUnits . status $ oldpr)
                 callParents oldpr (\x -> incrTotal x adjustment)
                 return $ oldpr {status = (status oldpr) 
                                 {totalUnits = completedUnits . status $ oldpr}}

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

> getSpeed progressobj >>= print

Don't let the type of this function confuse you.  It is a fancy way of saying
that it can take either a 'Progress' or a 'ProgressStatus' object, and returns
a number that is valid as any Fractional type, such as a Double, Float, or
Rational. -}
getSpeed :: (ProgressStatuses a (IO b), Fractional b) => a -> IO b
getSpeed po = withStatus po $ \status -> 
                do t <- timeSource status
                   let elapsed = t - (startTime status)
                   return $ if elapsed == 0
                       then fromRational 0
                       else fromRational ((completedUnits status) % elapsed)

{- | Returns the estimated time remaining, in standard time units. 

Returns 0 whenever 'getSpeed' would return 0.

See the comments under 'getSpeed' for information about this function's type
and result. -}
getETR :: (ProgressStatuses a (IO Integer),
           ProgressStatuses a (IO Rational)) => a -> IO Integer
getETR po = 
    do speed <- ((getSpeed po)::IO Rational)
       if speed == 0
          then return 0
          else 
              -- FIXME: potential for a race condition here, but it should
              -- be negligible
              withStatus po $ \status ->
                  do let remaining = totalUnits status - completedUnits status
                     return $ round $ (toRational remaining) / speed

{- | Returns the estimated system clock time of completion, in standard
time units.  Returns the current time whenever 'getETR' would return 0.

See the comments under 'getSpeed' for information about this function's type
and result. -}
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
          modfunc oldpr = 
              do let newpr = oldpr {status = func (status oldpr)}
                 mapM_ (\x -> x (status oldpr) (status newpr))
                           (callbacks oldpr)

                 -- Kick it up to the parents.
                 case (completedUnits . status $ newpr) -
                      (completedUnits . status $ oldpr) of
                   0 -> return ()
                   x -> callParents newpr (\y -> incrP' y x)
                 case (totalUnits . status $ newpr) -
                      (totalUnits . status $ oldpr) of
                   0 -> return ()
                   x -> callParents newpr (\y -> incrTotal y x)
                 return newpr

callParents :: ProgressRecord -> (Progress -> IO ()) -> IO ()
callParents pr func = mapM_ func (parents pr)

