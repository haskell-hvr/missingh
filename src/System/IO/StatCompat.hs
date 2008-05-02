{-# LANGUAGE CPP #-}
{-
Copyright (C) 2005,2006 John Goerzen <jgoerzen@complete.org>

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
   Module     : System.IO.StatCompat
   Copyright  : Copyright (C) 2005-2006 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Provide a stat-like structure for use in MissingH.  Especially
useful with HVFS and on Windows.  See also "System.IO.WindowsCompat".

Copyright (c) 2005-2006 John Goerzen, jgoerzen\@complete.org
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
