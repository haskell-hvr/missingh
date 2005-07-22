{-# LANGUAGE CPP #-}
{- Posix consts not included with Haskell
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
   Module     : MissingH.IO.PosixConsts
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Exports some POSIX constants and functions that are not exported in fptools
by default.

-}

module MissingH.IO.PosixConsts where
import System.Posix.Types
import Data.Bits

blockSpecialMode :: FileMode
blockSpecialMode = 0o0060000

characterSpecialMode :: FileMode
characterSpecialMode = 0o0020000

namedPipeMode :: FileMode
namedPipeMode = 0o0010000

regularFileMode :: FileMode
regularFileMode = 0o0100000

directoryMode :: FileMode
directoryMode = 0o0040000

fileTypeModes :: FileMode
fileTypeModes = 0o00170000

socketMode :: FileMode
socketMode = 0o0140000

symbolicLinkMode :: FileMode
symbolicLinkMode = 0o0120000


