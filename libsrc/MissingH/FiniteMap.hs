{- arch-tag: FiniteMap utilities main file
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
   Module     : MissingH.FiniteMap
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

This module provides various helpful utilities for dealing with FiniteMaps.

Written by John Goerzen, jgoerzen\@complete.org

In addition to the functions exported, this module also makes a FiniteMap
showable.
-}

module MissingH.FiniteMap (flipFM, flippedLookupFM)
where

import Data.FiniteMap
import MissingH.List(flipAL)

{- | Flips a finite map.  See 'MissingH.List.flipAL' for more on the similar
function for lists. -}

flipFM :: (Ord key, Ord val) => FiniteMap key val -> FiniteMap val [key]
flipFM = listToFM . flipAL . fmToList

{- | Returns a list of all keys in the finite map whose value matches the
parameter. If the value does not occur in the finite map, the empty
list is returned. -}

flippedLookupFM :: (Ord val, Ord key) => FiniteMap key val -> val-> [key]
flippedLookupFM fm v =
    case lookupFM (flipFM fm) v of
                                Nothing -> []
                                Just x -> x

{- | Makes a FiniteMap showable. -}
instance (Show a, Show b) => Show (FiniteMap a b) where
    show fm = show (fmToList fm)
