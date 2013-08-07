{-# LANGUAGE CPP #-}
{- Windows compatibility layer
Copyright (c) 2005-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : System.IO.WindowsCompat
   Copyright  : Copyright (C) 2005-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable

Provides some types and related items on Windows to be compatible with
the System.Posix.* libraries

See also "System.IO.StatCompat", which this module re-exports.

On non-Windows platforms, this module does nothing.

On Windows, it re-exports "System.IO.StatCompat".  It also provides various
file type information modes that are otherwise in "System.Posix.Types" or
"System.Posix.Files".  It also provides
a rudimentary implemention of getFileStatus that emulates the Posix call
to stat(2).

Common usage might be like this:

>import System.Posix.Types
>#if (defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
>import System.IO.WindowsCompat
>#else
>import System.Posix.Files
>#endif

Or, to avoid having to use CPP and make things even easier, just import
"System.IO.PlafCompat", which essentially does the above.

-}

module System.IO.WindowsCompat
#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
where
#else
       (module System.IO.StatCompat, module System.IO.WindowsCompat)
where

import System.Posix.Types
import Data.Bits
import System.IO.StatCompat
import System.Posix.Consts
import System.Time.Utils
import System.Directory
import Data.Time
import Data.Time.Clock.POSIX

-- these types aren't defined here

nullFileMode :: FileMode
nullFileMode = 0

ownerReadMode :: FileMode
ownerReadMode = 0o00400

ownerWriteMode :: FileMode
ownerWriteMode = 0o00200

ownerExecuteMode :: FileMode
ownerExecuteMode = 0o00100

groupReadMode :: FileMode
groupReadMode = 0o00040

groupWriteMode :: FileMode
groupWriteMode = 0o00020

groupExecuteMode :: FileMode
groupExecuteMode = 0o00010

otherReadMode :: FileMode
otherReadMode = 0o00004

otherWriteMode :: FileMode
otherWriteMode = 0o00002

otherExecuteMode :: FileMode
otherExecuteMode = 0o00001

setUserIDMode :: FileMode
setUserIDMode = 0o0004000

setGroupIDMode :: FileMode
setGroupIDMode = 0o0002000

stdFileMode :: FileMode
stdFileMode = ownerReadMode  .|. ownerWriteMode .|.
              groupReadMode  .|. groupWriteMode .|.
              otherReadMode  .|. otherWriteMode

ownerModes :: FileMode
ownerModes = 0o00700

groupModes :: FileMode
groupModes = 0o00070

otherModes :: FileMode
otherModes = 0o00007

accessModes :: FileMode
accessModes = ownerModes .|. groupModes .|. otherModes

utcTimeToSeconds :: Num a => UTCTime -> a
utcTimeToSeconds = fromInteger . floor . utcTimeToPOSIXSeconds

----------- stat
type FileStatus = FileStatusCompat
getFileStatus :: FilePath -> IO FileStatus
getFileStatus fp =
    do isfile <- doesFileExist fp
       isdir <- doesDirectoryExist fp
       perms <- getPermissions fp
       modct <- getModificationTime fp
#if MIN_VERSION_directory(1,2,0)
       let epochtime = utcTimeToSeconds modct
#else
       let epochtime = clockTimeToEpoch modct
#endif
       return $ FileStatusCompat {deviceID = -1,
                                  fileID = -1,
                                  fileMode = if isfile then regularFileMode
                                                       else directoryMode,
                                  linkCount = 1,
                                  fileOwner = 0,
                                  fileGroup = 0,
                                  specialDeviceID = -1,
                                  fileSize = 0, -- fixme: hFileSize?
                                  accessTime = fromInteger epochtime,
                                  modificationTime = fromInteger epochtime,
                                  statusChangeTime = fromInteger epochtime
                                 }
#endif
