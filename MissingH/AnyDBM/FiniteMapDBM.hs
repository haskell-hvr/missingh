{- arch-tag: Support for in-memory FiniteMaps as AnyDBM objects
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
   Module     : MissingH.AnyDBM.FiniteMapDBM
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen,
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Written by John Goerzen, jgoerzen\@complete.org

Support for working with FiniteMaps through the "MissingH.AnyDBM" framework.
-}

module MissingH.AnyDBM.FiniteMapDBM (FiniteMapDBM,
                                     newFiniteMapDBM,
                                     setFiniteMapDBM,
                                     getFiniteMapDBM
                                    )
where
import MissingH.AnyDBM
import Data.FiniteMap
import Data.IORef

{- | The type of the FiniteMapDBM. -}
type FiniteMapDBM = IORef (FiniteMap String String)

{- | Makes a new FiniteMapDBM with an empty FiniteMap. -}
newFiniteMapDBM :: IO FiniteMapDBM
newFiniteMapDBM = newIORef emptyFM

{- | Sets the embedded FiniteMap in this 'FiniteMapDBM' to the
given 'FiniteMap'. -}
setFiniteMapDBM :: FiniteMapDBM -> FiniteMap String String -> IO ()
setFiniteMapDBM h fm = writeIORef h fm

{- | Gets the embedded FiniteMap in this 'FiniteMapDBM'. -}
getFiniteMapDBM :: FiniteMapDBM -> IO (FiniteMap String String)
getFiniteMapDBM = readIORef

m = modifyIORef

instance AnyDBM FiniteMapDBM where
    insertA h k v = m h (\x -> addToFM x k v)
    deleteA h k = m h (\x -> delFromFM x k)
    lookupA h k = do fm <- readIORef h
                     return $ lookupFM fm k
    toListA h = do fm <- readIORef h
                   return $ fmToList fm
