{- arch-tag: Path utilities main file
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
   Module     : MissingH.Path
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

This module provides various helpful utilities for dealing with path and file
names.

Written by John Goerzen, jgoerzen\@complete.org
-}

module MissingH.Path(splitExt
                    )
where
import Data.List
import MissingH.List

{- | Splits a pathname into a tuple representing the root of the name and
the extension.  The extension is considered to be all characters from the last
dot after the last slash to the end.  Either returned string may be empty. -}

-- FIXME - See 6.4 API when released.

splitExt :: String -> (String, String)
splitExt path = 
    let dotindex = alwaysElemRIndex '.' path
        slashindex = alwaysElemRIndex '/' path
        in
        if dotindex <= slashindex
           then (path, "")
           else ((take dotindex path), (drop dotindex path))
