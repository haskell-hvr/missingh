{- arch-tag: HVFS main file
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
   Module     : MissingH.IO.HVFS
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Haskell Virtual FS -- generic support for real or virtual filesystem in Haskell

Copyright (c) 2004 John Goerzen, jgoerzen\@complete.org

-}

module MissingH.IO.HVFS(-- * Implementation Classes
                        HVFS, HVFSStat,
                        -- * Re-exported types from other modules
                        FilePath, DeviceID, FileID, FileMode, LinkCount,
                        UserID, GroupID, FileOffset, EpochTime,
                        
                    )
where

import MissingH.IO.HVIO
import System.IO
import System.Posix.Files
import System.Posix.Types
import System.Time

class HVFSStat b where
    vDeviceID :: b -> DeviceID
    vFileID :: b -> FileID
    vFileMode :: b -> FileMode
    vLinkCount :: b -> LinkCount
    vFileOwner :: b -> UserID
    vFileGroup :: b -> GroupID
    vSpecialDeviceID :: b -> DeviceID
    vFileSize :: b -> FileOffset
    vAccessTime :: b -> EpochTime
    vModificationTime :: b -> EpochTime
    vStatusChangeTime :: b -> EpochTime
    vIsBlockDevice :: b -> Bool
    vIsCharacterDevice :: b -> Bool
    vIsNamedPipe :: b -> Bool
    vIsRegularFile :: b -> Bool
    vIsDirectory :: b -> Bool
    vIsSymbolicLink :: b -> Bool
    vIsSocket :: b -> Bool

class HVFS a where
    vGetCurrentDirectory :: a -> IO FilePath
    vSetCurrentDirectory :: a -> FilePath -> IO ()
    vGetDirectoryContents :: a -> FilePath -> IO [FilePath]
    vDoesFileExist :: a -> FilePath -> IO Bool
    vDoesDirectoryExist :: a -> FilePath -> IO Bool
    vCreateDirectory :: a -> FilePath -> IO ()
    vRemoveDirectory :: a -> FilePath -> IO ()
    vRenameDirectory :: a -> FilePath -> FilePath -> IO ()
    vRemoveFile :: a -> FilePath -> IO ()
    vRenameFile :: a -> FilePath -> FilePath -> IO ()
    vGetFileStatus :: HVFSStat b => a -> FilePath -> IO b
    vGetSymbolicLinkStatus :: HVFSStat b => a -> FilePath -> IO b
    vGetModificationTime :: a -> FilePath -> IO ClockTime

class (HVFS a, HVIOGeneric b) => HVFSOpenable a b where
    vOpen :: a -> FilePath -> IO b

----------------------------------------------------------------------
-- Standard implementations
----------------------------------------------------------------------
