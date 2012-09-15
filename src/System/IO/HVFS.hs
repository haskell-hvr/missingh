{-# LANGUAGE CPP, ScopedTypeVariables, TypeSynonymInstances #-}
{- arch-tag: HVFS main file
Copyright (c) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : System.IO.HVFS
   Copyright  : Copyright (C) 2004-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable

Haskell Virtual FS -- generic support for real or virtual filesystem in Haskell

Copyright (c) 2004-2005 John Goerzen, jgoerzen\@complete.org

The idea of this module is to provide virtualization of filesystem calls.
In addition to the \"real\" system filesystem, you can also provide access
to other, virtual, filesystems using the same set of calls.  Examples of
such virtual filesystems might include a remote FTP server, WebDAV server,
a local Hashtable, a ConfigParser object, or any other data structure
you can represent as a tree of named nodes containing strings.

Each 'HVFS' function takes a 'HVFS' \"handle\" ('HVFS' instance) as its
first parameter.  If you wish to operate on the standard system filesystem,
you can just use 'SystemFS'.

The "MissingH.HVFS.IO.InstanceHelpers" module contains some code to help
you make your own HVFS instances.

The 'HVFSOpenable' class works together with the "System.IO.HVIO" module
to provide a complete virtual filesystem and I\/O model that allows you
to open up virtual filesystem files and act upon them in a manner similar
to standard Handles.
-}

module System.IO.HVFS(-- * Implementation Classes \/ Types
                        HVFS(..), HVFSStat(..), 
                        HVFSOpenable(..), HVFSOpenEncap(..),HVFSStatEncap(..),
                        withStat, withOpen,
                        SystemFS(..),
                        -- * Re-exported types from other modules
                        FilePath, DeviceID, FileID, FileMode, LinkCount,
                        UserID, GroupID, FileOffset, EpochTime,
                        IOMode
                    )
where

import qualified Control.Exception (catch, IOException)
import System.IO.HVIO
import System.Time.Utils
import System.IO
import System.IO.Error
import System.IO.PlafCompat
import System.Posix.Types
import System.Time
import System.Directory

#if MIN_VERSION_directory(1,2,0)
import Data.Time.Clock.POSIX ( utcTimeToPOSIXSeconds )
#endif

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
>          return $ epochToClockTime (withStat s vModificationTime)

See 'System.Time.Utils.epochToClockTime' for more information.
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

A default implementation of this is not currently present on Windows.
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
    vFileMode x = if vIsDirectory x then 0x755 else 0o0644
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

 * 'vDoesExist' -- implemented in terms of 'vGetSymbolicLinkStatus'

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
    {- | True if the file exists, regardless of what type it is.
       This is even True if the given path is a broken symlink. -}
    vDoesExist :: a -> FilePath -> IO Bool
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
           return $ epochToClockTime (withStat s vModificationTime)
    vRaiseError _ et desc mfp =
        ioError $ mkIOError et desc Nothing mfp

    vGetCurrentDirectory fs = eh fs "vGetCurrentDirectory"
    vSetCurrentDirectory fs _ = eh fs "vSetCurrentDirectory"
    vGetDirectoryContents fs _ = eh fs "vGetDirectoryContents"
    vDoesFileExist fs fp = 
        Control.Exception.catch (do s <- vGetFileStatus fs fp
                                    return $ withStat s vIsRegularFile
              ) (\(_ :: Control.Exception.IOException) -> return False)
    vDoesDirectoryExist fs fp = 
        Control.Exception.catch (do s <- vGetFileStatus fs fp
                                    return $ withStat s vIsDirectory
              ) (\(_ :: Control.Exception.IOException) -> return False)
    vDoesExist fs fp =
        Control.Exception.catch (do s <- vGetSymbolicLinkStatus fs fp
                                    return True
              ) (\(_ :: Control.Exception.IOException) -> return False)
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

{- | Types that can open a HVIO object should be instances of this class.
You need only implement 'vOpen'. -}

class HVFS a => HVFSOpenable a where
    vOpen :: a -> FilePath -> IOMode -> IO HVFSOpenEncap
    vReadFile :: a -> FilePath -> IO String
    vWriteFile :: a -> FilePath -> String -> IO ()
    vOpenBinaryFile :: a -> FilePath -> IOMode -> IO HVFSOpenEncap

    vReadFile h fp = 
        do oe <- vOpen h fp ReadMode
           withOpen oe (\fh -> vGetContents fh)

    vWriteFile h fp s =
        do oe <- vOpen h fp WriteMode
           withOpen oe (\fh -> do vPutStr fh s
                                  vClose fh)

    -- | Open a file in binary mode.
    vOpenBinaryFile = vOpen

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
#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
    vGetSymbolicLinkStatus _ fp = getSymbolicLinkStatus fp >>= return . HVFSStatEncap
#else
    -- No symlinks on Windows; just get the file status directly
    vGetSymbolicLinkStatus = vGetFileStatus
#endif

#if MIN_VERSION_directory(1,2,0)
    vGetModificationTime _ p = getModificationTime p >>= (\modUTCTime -> return $ TOD ((toEnum . fromEnum . utcTimeToPOSIXSeconds) modUTCTime) 0)
#else
    vGetModificationTime _ = getModificationTime
#endif
#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
    vCreateSymbolicLink _ = createSymbolicLink
    vReadSymbolicLink _ = readSymbolicLink
    vCreateLink _ = createLink
#else
    vCreateSymbolicLink _ _ _ = fail "Symbolic link creation not supported by Windows"
    vReadSymbolicLink _ _ = fail "Symbolic link reading not supported by Widnows"
    vCreateLink _ _ _ = fail "Hard link creation not supported by Windows"
#endif

instance HVFSOpenable SystemFS where
    vOpen _ fp iomode = openFile fp iomode >>= return . HVFSOpenEncap
    vOpenBinaryFile _ fp iomode = openBinaryFile fp iomode >>= return . HVFSOpenEncap

