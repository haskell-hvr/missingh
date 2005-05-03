-- arch-tag: FiniteMap utilities main file
{-# LANGUAGE CPP #-}
{- Copyright (C) 2004-2005 John Goerzen <jgoerzen@complete.org>

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
   Copyright  : Copyright (C) 2004-2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

This module provides various helpful utilities for dealing with FiniteMaps.

Written by John Goerzen, jgoerzen\@complete.org

In addition to the functions exported, this module also makes a FiniteMap
showable on GHC prior to 6.4.  (GHC 6.4 and current Hugs versions have it
showable already).
-}

module MissingH.FiniteMap (-- * Basic Utilities
                           flipFM, flippedLookupFM, forceLookupFM,
                           -- * Conversions
                           strToFM,
                           strFromFM
                          )
where

import Data.FiniteMap
import MissingH.List(flipAL, strToAL, strFromAL)

{- | Converts a String, String FiniteMap into a string representation.
See 'MissingH.List.strFromAL' for more on the similar function for
association lists.  This implementation is simple:

>strFromFM = strFromAL . fmToList

This function is designed to work with FiniteMap String String objects,
but may also work with other objects with simple representations. -}
strFromFM :: (Show a, Show b, Ord a) => FiniteMap a b -> String
strFromFM = strFromAL . fmToList

{- | Converts a String into a String, String FiniteMap.  See
'MissingH.List.strToAL' for more on the similar function for association
lists.

This implementation is simple:

>strToFM = listToFM . strToAL

This function is designed to work with FiniteMap String String objects,
but may work with other key\/value combinations if they have simple
representations.  -}
strToFM :: (Read a, Read b, Ord a) => String -> FiniteMap a b
strToFM = listToFM . strToAL

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

#if __GLASGOW_HASKELL__ >= 603 || __HUGS__
{- FiniteMap is already showable on this platform -}
#else
{- | Makes a FiniteMap showable. -}
instance (Show a, Show b) => Show (FiniteMap a b) where
    show fm = show (fmToList fm)
#endif

{- | Performs a lookup, and raises an exception (with an error message
prepended with the given string) if the key could not be found.
-}
forceLookupFM :: (Show key, Ord key) => String -> FiniteMap key elt -> key -> elt
forceLookupFM msg fm k =
    case lookupFM fm k of
         Just x -> x
         Nothing -> error $ msg ++ ": could not find key " ++ (show k)
