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
import System.IO.Error
import System.Posix.Files
import System.Posix.Types
import System.Time

class HVFSStat a where
    vDeviceID :: a -> DeviceID
    vFileID :: a -> FileID
    vFileMode :: a -> FileMode
    vLinkCount :: a -> LinkCount
    vFileOwner :: a -> UserID
    vFileGroup :: a -> GroupID
    vSpecialDeviceID :: a -> DeviceID
    vFileSize :: a -> FileOffset
    vAccessTime :: a -> EpochTime
    vModificationTime :: a -> EpochTime
    vStatusChangeTime :: a -> EpochTime
    vIsBlockDevice :: a -> Bool
    vIsCharacterDevice :: a -> Bool
    vIsNamedPipe :: a -> Bool
    vIsRegularFile :: a -> Bool
    vIsDirectory :: a -> Bool
    vIsSymbolicLink :: a -> Bool
    vIsSocket :: a -> Bool

{- | The main HVFS class.

A default implementation of 'vGetModificationTime' is provided (in terms
of 'vGetFileStatus').  A standard implementation of 'vRaiseError' is also
provided.

Default implementations of all other functions
will generate an isIllegalOperation error, since they are assumed to be
un-implemented. -}
class HVFSStat b => HVFS a b where
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
    vGetFileStatus :: a -> FilePath -> IO b
    vGetSymbolicLinkStatus :: a -> FilePath -> IO b
    vGetModificationTime :: a -> FilePath -> IO ClockTime
    vRaiseError :: a -> IOErrorType -> String -> Maybe FilePath -> IO c

    vGetModificationTime fs fp = 
        do s <- (vGetFileStatus fs fp)::IO b
           let t = vModificationTime s
           return $ TOD (fromIntegral t) 0

    vRaiseError fs et desc mfp =
        ioError $ mkIOError et desc Nothing mfp

    --vGetCurrentDirectory fs = vRaiseError fs 

class (HVFS a b, HVIOGeneric c) => HVFSOpenable a b c where
    vOpen :: a -> FilePath -> IO c

----------------------------------------------------------------------
-- Standard implementations
----------------------------------------------------------------------
