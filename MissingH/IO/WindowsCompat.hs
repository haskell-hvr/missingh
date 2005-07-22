{-# LANGUAGE CPP #-}
{- Windows compatibility layer
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

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
   Module     : MissingH.IO.WindowsCompat
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Provides some types and related items on Windows to be compatible with
the System.Posix.* libraries

See also "MissingH.IO.StatCompat", which this module re-exports.

Copyright (c) 2005 John Goerzen, jgoerzen\@complete.org

On non-Windows platforms, this module does nothing.

On Windows, it re-exports "MissingH.IO.StatCompat".  It also provides various
file type information modes that are otherwise in "System.Posix.Types" or
"System.Posix.Files".  It also provides
a rudimentary implemention of getFileStatus that emulates the Posix call
to stat(2).

Common usage might be like this:

>import System.Posix.Types
>#ifdef mingw32_HOST_OS
>import MissingH.IO.WindowsCompat
>#else
>import System.Posix.Files
>#endif

Or, to avoid having to use CPP and make things even easier, just import
"MissingH.IO.PlafCompat", which essentially does the above.

-}

module MissingH.IO.WindowsCompat
#ifndef mingw32_HOST_OS
where
#else
       (module MissingH.IO.StatCompat, module MissingH.IO.WindowsCompat)
where

import System.Posix.Types
import Data.Bits
import MissingH.IO.StatCompat
import MissingH.IO.PosixConsts
import MissingH.Time
import System.Directory

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

----------- stat
type FileStatus = FileStatusCompat
getFileStatus :: FilePath -> IO FileStatus
getFileStatus fp =
    do isfile <- doesFileExist fp
       isdir <- doesDirectoryExist fp
       perms <- getPermissions fp
       modct <- getModificationTime fp
       let epochtime = clockTimeToEpoch modct
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
