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
   Module     : MissingH.Map
   Copyright  : Copyright (C) 2004-2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable

This module provides various helpful utilities for dealing with Data.Maps.

Written by John Goerzen, jgoerzen\@complete.org
-}

module MissingH.Map (-- * Basic Utilities
                     flipM, flippedLookupM, forceLookupM,
                     -- * Conversions
                     strToM,
                     strFromM
                          )
where

import qualified Data.Map
import MissingH.List(flipAL, strToAL, strFromAL)

{- | Converts a String, String Map into a string representation.
See 'MissingH.List.strFromAL' for more on the similar function for
association lists.  This implementation is simple:

>strFromM = strFromAL . Data.Map.toList

This function is designed to work with Map String String objects,
but may also work with other objects with simple representations. -}
strFromM :: (Show a, Show b, Ord a) => Data.Map.Map a b -> String
strFromM = strFromAL . Data.Map.toList

{- | Converts a String into a String, String Map.  See
'MissingH.List.strToAL' for more on the similar function for association
lists.

This implementation is simple:

>strToM = Data.Map.fromList . strToAL

This function is designed to work with FiniteMap String String objects,
but may work with other key\/value combinations if they have simple
representations.  -}
strToM :: (Read a, Read b, Ord a) => String -> Data.Map.Map a b
strToM = Data.Map.fromList . strToAL

{- | Flips a Map.  See 'MissingH.List.flipAL' for more on the similar
function for lists. -}

flipM :: (Ord key, Ord val) => Data.Map.Map key val -> Data.Map.Map val [key]
flipM = Data.Map.fromList . flipAL . Data.Map.toList

{- | Returns a list of all keys in the Map whose value matches the
parameter. If the value does not occur in the Map, the empty
list is returned. -}

flippedLookupM :: (Ord val, Ord key) => val -> Data.Map.Map key val -> [key]
flippedLookupM v fm =
    case Data.Map.lookup v (flipM fm) of
                             Nothing -> []
                             Just x -> x

{- | Performs a lookup, and raises an exception (with an error message
prepended with the given string) if the key could not be found.
-}
forceLookupM :: (Show key, Ord key) => String -> key ->
                                       Data.Map.Map key elt -> elt
forceLookupM msg k fm =
    case Data.Map.lookup k fm of
         Just x -> x
         Nothing -> error $ msg ++ ": could not find key " ++ (show k)
