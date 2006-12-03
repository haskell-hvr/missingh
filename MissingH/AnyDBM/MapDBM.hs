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
   Module     : Database.AnyDBM.MapDBM
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Written by John Goerzen, jgoerzen\@complete.org

Support for working with Maps through the "Database.AnyDBM" framework.
-}

module Database.AnyDBM.MapDBM (MapDBM,
                               newMapDBM,
                               setMapDBM,
                               getMapDBM
                                    )
where
import Database.AnyDBM
import Data.Map as Map
import Control.Concurrent.MVar

{- | The type of the MapDBM. -}
type MapDBM = MVar (Map.Map String String)

{- | Makes a new MapDBM with an empty Map. -}
newMapDBM :: IO MapDBM
newMapDBM = newMVar Map.empty

{- | Sets the embedded Map in this 'MapDBM' to the
given 'Map'. -}
setMapDBM :: MapDBM -> Map.Map String String -> IO ()
setMapDBM h fm = swapMVar h fm >> return ()

{- | Gets the embedded Map in this 'MapDBM'. -}
getMapDBM :: MapDBM -> IO (Map.Map String String)
getMapDBM = readMVar

m = modifyMVar_

instance AnyDBM MapDBM where
    insertA h k v = m h (\x -> return $ Map.insert k v x)
    deleteA h k = m h (\x -> return $ Map.delete k x)
    lookupA h k = withMVar h (\x -> return $ Map.lookup k x)
    toListA h = withMVar h (\x -> return $ Map.toList x)
