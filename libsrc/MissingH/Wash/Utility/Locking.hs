module MissingH.Wash.Utility.Locking (obtainLock, releaseLock) where

import MissingH.Wash.Utility.Auxiliary
import Directory
import IO
import System
import Time

obtainLock  :: FilePath -> IO ()
releaseLock :: FilePath -> IO ()

lockPath name = name ++ ".lockdir"

obtainLock name =
  assertDirectoryExists (lockPath name)
                        (system "sleep 1" >> obtainLockLoop name)

releaseLock name =
  removeDirectory (lockPath name)

obtainLockLoop name =
  let lp = lockPath name in
  do b <- doesDirectoryExist lp
     if b then do -- check if lock is stale
		  mtime <- getModificationTime lp
		  ftime <- getModificationTime name
		  ctime <- getClockTime
		  let td = diffClockTimes ctime mtime
		      tf = diffClockTimes ctime ftime
		  if tdSec td > 60 && tdSec tf > 60
		    then do removeDirectory lp
			    obtainLock name
		    else do system "sleep 1"
			    obtainLockLoop name
		    
          else obtainLock name
