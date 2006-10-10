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

module MissingH.ProgressTracker (ProgressRecord(completedUnits, totalUnits,
                                                startTime, trackerName,
                                                timeSource),
                                 Progress
                               )

where
import Control.Concurrent.MVar

type ProgressTimeSource = IO Integer
type ProgressCallback = ProgressRecord -> IO ()

{- | The main progress status unit. -}
data ProgressRecord = 
     ProgressRecord {completedUnits :: Integer,
                     totalUnits :: Integer,
                     startTime :: Integer,
                     trackerName :: String,
                     timeSource :: ProgressTimeSource,
                     parents :: [Progress],
                     callbacks :: [ProgressCallback]}

newtype Progress = Progress (MVar ProgressRecord)

class ProgressTypes a where
    withStatus :: a -> (ProgressRecord -> IO b) -> IO b

instance ProgressTypes ProgressRecord where
    withStatus x func = func x

instance ProgressTypes (MVar ProgressRecord) where
    withStatus x func = withMVar x func

now :: ProgressTypes a => ProgressTimeSource
now x = withStatus x timeSource

