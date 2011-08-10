{- 
Copyright (c) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : Data.Map.Utils
   Copyright  : Copyright (C) 2004-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable

This module provides various helpful utilities for dealing with Data.Maps.

Written by John Goerzen, jgoerzen\@complete.org
-}

module Data.Map.Utils (-- * Basic Utilities
                     flipM, flippedLookupM, forceLookupM,
                     -- * Conversions
                     strToM,
                     strFromM
                          )
where

import qualified Data.Map
import Data.List.Utils(flipAL, strToAL, strFromAL)

{- | Converts a String, String Map into a string representation.
See 'Data.List.Utils.strFromAL' for more on the similar function for
association lists.  This implementation is simple:

>strFromM = strFromAL . Data.Map.toList

This function is designed to work with Map String String objects,
but may also work with other objects with simple representations. -}
strFromM :: (Show a, Show b, Ord a) => Data.Map.Map a b -> String
strFromM = strFromAL . Data.Map.toList

{- | Converts a String into a String, String Map.  See
'Data.List.Utils.strToAL' for more on the similar function for association
lists.

This implementation is simple:

>strToM = Data.Map.fromList . strToAL

This function is designed to work with Map String String objects,
but may work with other key\/value combinations if they have simple
representations.  -}
strToM :: (Read a, Read b, Ord a) => String -> Data.Map.Map a b
strToM = Data.Map.fromList . strToAL

{- | Flips a Map.  See 'Data.List.Utils.flipAL' for more on the similar
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
