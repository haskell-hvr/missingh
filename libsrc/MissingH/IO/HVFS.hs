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
                        HVFS(..), HVFSStat(..), 
                        HVFSOpenable(..), HVFSOpenEncap(..),HVFSStatEncap(..),
                        withStat, withOpen,
                        SystemFS(..),
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

{- | Encapsulate a 'HVFSStat' result.  This is required due to Haskell
typing restrictions.  You can get at it with:

> case encap of
>    HVFSStatEncap x -> -- now use x
-}
data HVFSStatEncap = forall a. HVFSStat a => HVFSStatEncap a

{- | Convenience function for working with stat -- takes a stat result
and a function that uses it, and returns the result. 

Here is an example from the HVFS source:

>    vGetModificationTime fs fp = 
>       do s <- vGetFileStatus fs fp
>          return $ TOD (fromIntegral (withStat s vModificationTime)) 0
-}
withStat :: forall b. HVFSStatEncap -> (forall a. HVFSStat a => a -> b) -> b
withStat s f =
    case s of
           HVFSStatEncap x -> f x

{- | Similar to 'HVFSStatEncap', but for 'vOpen' result.
-}
data HVFSOpenEncap = forall a. HVIO a => HVFSOpenEncap a

{- | Similar to 'withStat', but for the 'vOpen' result. -}
withOpen :: forall b. HVFSOpenEncap -> (forall a. HVIO a => a -> b) -> b
withOpen s f =
    case s of
           HVFSOpenEncap x -> f x

{- | Evaluating types of files and information about them.

This corresponds to the System.Posix.Types.FileStatus type, and indeed,
that is one instance of this class.

Inplementators must, at minimum, implement 'vIsDirectory' and
'vIsRegularFile'.

Default implementations of everything else are provided, returning
reasonable values.
-}
class (Show a) => HVFSStat a where
    vDeviceID :: a -> DeviceID
    vFileID :: a -> FileID
    {- | Refers to file permissions, NOT the st_mode field from stat(2) -}
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

    vDeviceID _ = 0
    vFileID _ = 0
    vFileMode _ = 0o0644
    vLinkCount _ = 1
    vFileOwner _ = 0
    vFileGroup _ = 0
    vSpecialDeviceID _ = 0
    vFileSize _ = 0
    vAccessTime _ = 0
    vModificationTime _ = 0
    vStatusChangeTime _ = 0
    vIsBlockDevice _ = False
    vIsCharacterDevice _ = False
    vIsNamedPipe _ = False
    vIsSymbolicLink _ = False
    vIsSocket _ = False

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

Most of these functions correspond to functions in System.Directory or
System.Posix.Files.  Please see detailed documentation on them there.
 -}
class (Show a) => HVFS a where
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
    {- | Raise an error relating to actions on this class. -}
    vRaiseError :: a -> IOErrorType -> String -> Maybe FilePath -> IO c
    vCreateSymbolicLink :: a -> FilePath -> FilePath -> IO ()
    vReadSymbolicLink :: a -> FilePath -> IO FilePath
    vCreateLink :: a -> FilePath -> FilePath -> IO ()

    vGetModificationTime fs fp = 
        do s <- vGetFileStatus fs fp
           return $ TOD (fromIntegral (withStat s vModificationTime)) 0
    vRaiseError _ et desc mfp =
        ioError $ mkIOError et desc Nothing mfp

    vGetCurrentDirectory fs = eh fs "vGetCurrentDirectory"
    vSetCurrentDirectory fs _ = eh fs "vSetCurrentDirectory"
    vGetDirectoryContents fs _ = eh fs "vGetDirectoryContents"
    vDoesFileExist fs fp = 
        catch (do s <- vGetFileStatus fs fp
                  return $ withStat s vIsRegularFile
              ) (\_ -> return False)
    vDoesDirectoryExist fs fp = 
        catch (do s <- vGetFileStatus fs fp
                  return $ withStat s vIsDirectory
              ) (\_ -> return False)
    vCreateDirectory fs _ = eh fs "vCreateDirectory"
    vRemoveDirectory fs _ = eh fs "vRemoveDirectory"
    vRemoveFile fs _ = eh fs "vRemoveFile"
    vRenameFile fs _ _ = eh fs "vRenameFile"
    vRenameDirectory fs _ _ = eh fs "vRenameDirectory"
    vCreateSymbolicLink fs _ _ = eh fs "vCreateSymbolicLink"
    vReadSymbolicLink fs _ = eh fs "vReadSymbolicLink"
    vCreateLink fs _ _ = eh fs "vCreateLink"
    vGetSymbolicLinkStatus = vGetFileStatus

-- | Error handler helper
eh :: HVFS a => a -> String -> IO c
eh fs desc = vRaiseError fs illegalOperationErrorType 
             (desc ++ " is not implemented in this HVFS class") Nothing

class HVFS a => HVFSOpenable a where
    vOpen :: a -> FilePath -> IOMode -> IO HVFSOpenEncap

instance Show FileStatus where
    show _ = "<FileStatus>"

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
    vCreateSymbolicLink _ = createSymbolicLink
    vReadSymbolicLink _ = readSymbolicLink
    vCreateLink _ = createLink

instance HVFSOpenable SystemFS where
    vOpen _ fp iomode = openFile fp iomode >>= return . HVFSOpenEncap
