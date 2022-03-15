{-# LANGUAGE CPP  #-}
{-# LANGUAGE Safe #-}
{-
Copyright (c) 2005-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : System.IO.StatCompat
   Copyright  : Copyright (C) 2005-2011 John Goerzen
   SPDX-License-Identifier: BSD-3-Clause

   Stability  : stable
   Portability: portable

Provide a stat-like structure for use in MissingH.  Especially
useful with HVFS and on Windows.  See also "System.IO.WindowsCompat".

-}

module System.IO.StatCompat where

import safe System.Posix.Consts
    ( blockSpecialMode,
      characterSpecialMode,
      namedPipeMode,
      regularFileMode,
      directoryMode,
      fileTypeModes,
      socketMode,
      symbolicLinkMode )
import safe System.Posix.Types
    ( DeviceID,
      EpochTime,
      FileID,
      FileMode,
      FileOffset )

#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
import safe System.Posix.Types ( LinkCount, UserID, GroupID )
import safe System.Posix.Files ( intersectFileModes )
#else
import safe Data.Bits          ( (.&.) )

type LinkCount = Int
type UserID = Int
type GroupID = Int

intersectFileModes :: FileMode -> FileMode -> FileMode
intersectFileModes m1 m2 = m1 .&. m2
#endif

data FileStatusCompat =
    FileStatusCompat {deviceID         :: DeviceID,
                      fileID           :: FileID,
                      fileMode         :: FileMode,
                      linkCount        :: LinkCount,
                      fileOwner        :: UserID,
                      fileGroup        :: GroupID,
                      specialDeviceID  :: DeviceID,
                      fileSize         :: FileOffset,
                      accessTime       :: EpochTime,
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
