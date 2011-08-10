{-# LANGUAGE CPP #-}
{-
Copyright (c) 2005-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : System.IO.StatCompat
   Copyright  : Copyright (C) 2005-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Provide a stat-like structure for use in MissingH.  Especially
useful with HVFS and on Windows.  See also "System.IO.WindowsCompat".

-}

module System.IO.StatCompat
where
import System.Posix.Types
import System.Posix.Consts
#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
import System.Posix.Files(intersectFileModes)
#endif
import Data.Bits ((.&.))

#if (defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
type LinkCount = Int
type UserID = Int
type GroupID = Int
#endif

data FileStatusCompat =
    FileStatusCompat {deviceID :: DeviceID,
                      fileID :: FileID,
                      fileMode :: FileMode,
                      linkCount :: LinkCount,
                      fileOwner :: UserID,
                      fileGroup :: GroupID,
                      specialDeviceID :: DeviceID,
                      fileSize :: FileOffset,
                      accessTime :: EpochTime,
                      modificationTime :: EpochTime,
                      statusChangeTime :: EpochTime
                     }

sc_helper :: FileMode -> FileStatusCompat -> Bool
sc_helper comp stat =
    (fileMode stat `intersectFileModes` fileTypeModes) == comp

isBlockDevice,isCharacterDevice,isNamedPipe,isRegularFile,isDirectory,isSymbolicLink,isSocket :: FileStatusCompat -> Bool
isBlockDevice = sc_helper blockSpecialMode
isCharacterDevice = sc_helper characterSpecialMode
isNamedPipe = sc_helper namedPipeMode
isRegularFile = sc_helper regularFileMode
isDirectory = sc_helper directoryMode
isSymbolicLink = sc_helper symbolicLinkMode
isSocket = sc_helper socketMode

#if (defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
intersectFileModes :: FileMode -> FileMode -> FileMode
intersectFileModes m1 m2 = m1 .&. m2
#endif
