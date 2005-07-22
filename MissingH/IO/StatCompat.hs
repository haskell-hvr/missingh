{-# LANGUAGE CPP #-}
{- 
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
   Module     : MissingH.IO.StatCompat
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Provide a stat-like structure for use in MissingH.  Especially
useful with HVFS and on Windows.  See also "MissingH.IO.WindowsCompat".

Copyright (c) 2005 John Goerzen, jgoerzen\@complete.org
-}

module MissingH.IO.StatCompat 
where
import System.Posix.Types
import MissingH.IO.PosixConsts
import Data.Bits
#ifndef mingw32_HOST_OS
import System.Posix.Files(intersectFileModes)
#endif

#ifdef mingw32_HOST_OS
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
    
sc_helper comp stat = 
    (fileMode stat `intersectFileModes` fileTypeModes) == comp
                                      
isBlockDevice = sc_helper blockSpecialMode
isCharacterDevice = sc_helper characterSpecialMode
isNamedPipe = sc_helper namedPipeMode
isRegularFile = sc_helper regularFileMode
isDirectory = sc_helper directoryMode
isSymbolicLink = sc_helper symbolicLinkMode
isSocket = sc_helper socketMode

#ifdef mingw32_HOST_OS
intersectFileModes :: FileMode -> FileMode -> FileMode
intersectFileModes m1 m2 = m1 .&. m2
#endif
