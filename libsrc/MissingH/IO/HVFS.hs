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

module MissingH.IO.HVFS(-- * Implementation Classes \/ Types
                        HVFS(..), HVFSStat(..), HVFSStatEncap(..),
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
import System.Directory

data HVFSStatEncap = forall a. HVFSStat a => HVFSStatEncap a

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

Default implementations of these functions are provided:

 * 'vGetModificationTime' -- implemented in terms of 'vGetFileStatus'

 * 'vRaiseError'

 * 'vDoesFileExist' -- implemented in terms of 'vGetFileStatus'

 * 'vDoesDirectoryExist' -- implemented in terms of 'vGetFileStatus'

 * 'vGetSymbolicLinkStatus' -- set to call 'vGetFileStatus'.

Default implementations of all other functions
will generate an isIllegalOperation error, since they are assumed to be
un-implemented.

You should always provide at least a 'vGetFileStatus' call, and almost
certainly several of the others.
 -}
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
    vGetFileStatus :: a -> FilePath -> IO HVFSStatEncap
    vGetSymbolicLinkStatus :: a -> FilePath -> IO HVFSStatEncap
    vGetModificationTime :: a -> FilePath -> IO ClockTime
    vRaiseError :: a -> IOErrorType -> String -> Maybe FilePath -> IO c

    vGetModificationTime fs fp = 
        do s <- vGetFileStatus fs fp
           case s of
                  HVFSStatEncap x -> return $ 
                                      TOD (fromIntegral (vModificationTime x)) 0
    vRaiseError _ et desc mfp =
        ioError $ mkIOError et desc Nothing mfp

    vGetCurrentDirectory fs = eh fs "vGetCurrentDirectory"
    vSetCurrentDirectory fs _ = eh fs "vSetCurrentDirectory"
    vGetDirectoryContents fs _ = eh fs "vGetDirectoryContents"
    vDoesFileExist fs fp = 
        catch (do s <- vGetFileStatus fs fp
                  case s of
                     HVFSStatEncap x -> return $ vIsRegularFile x
              ) (\_ -> return False)
    vDoesDirectoryExist fs fp = 
        catch (do s <- vGetFileStatus fs fp
                  case s of
                     HVFSStatEncap x -> return $ vIsDirectory x
              ) (\_ -> return False)
    vCreateDirectory fs _ = eh fs "vCreateDirectory"
    vRemoveDirectory fs _ = eh fs "vRemoveDirectory"
    vRemoveFile fs _ = eh fs "vRemoveFile"
    vRenameFile fs _ _ = eh fs "vRenameFile"
    vGetSymbolicLinkStatus = vGetFileStatus

-- | Error handler helper
eh :: HVFS a => a -> String -> IO c
eh fs desc = vRaiseError fs illegalOperationErrorType 
             (desc ++ " is not implemented in this HVFS class") Nothing

class (HVFS a, HVIOGeneric c) => HVFSOpenable a b c where
    vOpen :: a -> FilePath -> IO c

----------------------------------------------------------------------
-- Standard implementations
----------------------------------------------------------------------
instance HVFSStat FileStatus where
    vDeviceID = deviceID
    vFileID = fileID
    vFileMode = fileMode
    vLinkCount = linkCount
    vFileOwner = fileOwner
    vFileGroup = fileGroup
    vSpecialDeviceID = specialDeviceID
    vFileSize = fileSize
    vAccessTime = accessTime
    vModificationTime = modificationTime
    vStatusChangeTime = statusChangeTime
    vIsBlockDevice = isBlockDevice
    vIsCharacterDevice = isCharacterDevice
    vIsNamedPipe = isNamedPipe
    vIsRegularFile = isRegularFile
    vIsDirectory = isDirectory
    vIsSymbolicLink = isSymbolicLink
    vIsSocket = isSocket

data SystemFS = SystemFS
              deriving (Eq, Show)

instance HVFS SystemFS where
    vGetCurrentDirectory _ = getCurrentDirectory
    vSetCurrentDirectory _ = setCurrentDirectory
    vGetDirectoryContents _ = getDirectoryContents
    vDoesFileExist _ = doesFileExist
    vDoesDirectoryExist _ = doesDirectoryExist
    vCreateDirectory _ = createDirectory
    vRemoveDirectory _ = removeDirectory
    vRenameDirectory _ = renameDirectory
    vRemoveFile _ = removeFile
    vRenameFile _ = renameFile
    vGetFileStatus _ fp = getFileStatus fp >>= return . HVFSStatEncap
    vGetSymbolicLinkStatus _ fp = getSymbolicLinkStatus fp >>= return . HVFSStatEncap
    vGetModificationTime _ = getModificationTime
