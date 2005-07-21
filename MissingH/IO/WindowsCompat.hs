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

Copyright (c) 2005 John Goerzen, jgoerzen\@complete.org

-}

module MissingH.IO.WindowsCompat()
where

import System.Posix.Types
import Data.Bits

#ifdef mingw32_HOST_OS
-- these types aren't defined here
type LinkCount = Int
type UserID = Int
type GroupID = Int
#endif

nullFileMode :: FileMode
nullFileMode = 0

ownerReadMode :: FileMode
ownerReadMode = 00400

ownerWriteMode :: FileMode
ownerWriteMode = 00200

ownerExecuteMode :: FileMode
ownerExecuteMode :: 00100

groupReadMode :: FileMode
groupReadMode = 00040

groupWriteMode :: FileMode
groupWriteMode = 00020

groupExecuteMode :: FileMode
groupExecuteMode = 00010

otherReadMode :: FileMode
otherReadMode = 00004

otherWriteMode :: FileMode
otherWriteMode = 00002

otherExecuteMode :: FileMode
otherExecuteMode = 00001

setUserIDMode :: FileMode
setUserIDMode = 0004000

setGroupIDMode :: FileMode
setGroupIDMode = 0002000

stdFileMode :: FileMode
stdFileMode = ownerReadMode  .|. ownerWriteMode .|.
              groupReadMode  .|. groupWriteMode .|.
              otherReadMode  .|. otherWriteMode

ownerModes :: FileMode
ownerModes = 00700

groupModes :: FileMode
groupModes = 00070

otherModes :: FileMode
otherModes :: 00007

accessModes :: FileMode
accessModes = ownerModes .|. groupModes .|. otherModes

