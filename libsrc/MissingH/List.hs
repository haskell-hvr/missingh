{- arch-tag: List utilities main file
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
   Module     : MissingH.List
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

This module provides various helpful utilities for dealing with lists.

Written by John Goerzen, jgoerzen\@complete.org
-}

module MissingH.List(-- * Tests
                     startswith, endswith, contains,
                     -- * Association List Utilities
                     {- | These functions are designed to augment the
                     association list functions in "Data.List" and
                     provide an interface similar to "Data.FiniteMap"
                     for association lists. -}
                     addToAL, delFromAL, flipAL,
                     -- * Conversions
                     split, join, genericJoin, trunc,
                     -- * Miscellaneous
                     countElem, elemRIndex, alwaysElemRIndex
                     -- -- * Sub-List Selection
                     -- sub,
                    ) where
import Data.List(intersperse, concat, isPrefixOf, isSuffixOf, elemIndices,
                elemIndex, elemIndices)
import IO
import System.IO.Unsafe

{- | Returns true if the given list starts with the specified elements;
false otherwise.  (This is an alias for "Data.List.isPrefixOf".)

Example:

> startswith "He" "Hello" -> True

-}

startswith :: Eq a => [a] -> [a] -> Bool
startswith = isPrefixOf

{- | Returns true if the given list ends with the specified elements;
false otherwise.  (This is an alias for "Data.List.isSuffixOf".)

Example:

> endswith "lo" "Hello" -> True

-}
endswith :: Eq a => [a] -> [a] -> Bool
endswith = isSuffixOf

{- | Given a delimiter and a list (or string), split into components.

Example:

> split "," "foo,bar,,baz," -> ["foo", "bar", "", "baz", ""]

> split "ba" ",foo,bar,,baz," -> [",foo,","r,,","z,"]
-}
split :: Eq a => [a] -> [a] -> [[a]]
split delim str =
    let splitworker :: Eq a => [a] -> [a] -> [a] -> [[a]]
        splitworker delim [] [] = []
        splitworker delim [] accum = [accum]
        splitworker delim str accum =
            if delim == str then 
               accum : [] : []
            else if startswith delim str then
               accum : splitworker delim (drop (length delim) str) []
            else splitworker delim (tail str) (accum ++ [head str])
        in
        splitworker delim str []

{- | Given a delimiter and a list of items (or strings), join the items
by using the delimiter.

Example:

> join "|" ["foo", "bar", "baz"] -> "foo|bar|baz"
-}
join :: [a] -> [[a]] -> [a]
join delim l = concat (intersperse delim l)

{- | Like 'join', but works with a list of anything showable, converting
it to a String.

Examples:

> genericJoin ", " [1, 2, 3, 4] -> "1, 2, 3, 4"
> genericJoin "|" ["foo", "bar", "baz"] -> "\"foo\"|\"bar\"|\"baz\""

-}
genericJoin :: Show a => String -> [a] -> String
genericJoin delim l = join delim (map show l)

{- | Returns true if the given parameter is a sublist of the given list;
false otherwise.

Example:

> contains "Haskell" "I really like Haskell." -> True
> contains "Haskell" "OCaml is great." -> False
-}

contains :: Eq a => [a] -> [a] -> Bool
contains [] _ = True                    -- Sub is empty; matches anything
contains _ [] = False                   -- List is empty; matches nothing
contains sub searchlist =
    let testlist = take (length sub) searchlist
        in
        case sub == testlist of
                             True -> True
                             False -> contains sub (tail searchlist)

{- | Given a length and a list, remove any elements at the end of the list
that make it longer than the length.  If the list is shorter than the
length, do nothing.

> trunc 2 "Hello" -> "He"
-}

trunc :: Int -> [a] -> [a]
trunc maxlen list =
    let removecount = (length list) - maxlen in
        if (removecount < 1) then list
        else reverse $ drop removecount $ reverse list

{- | Adds the specified (key, value) pair to the given list, removing any
existing pair with the same key already present. -}
addToAL :: Eq key => [(key, elt)] -> key -> elt -> [(key, elt)]
addToAL l key value = (key, value) : delFromAL l key

{- | Removes all (key, value) pairs from the given list where the key
matches the given one. -}
delFromAL :: Eq key => [(key, a)] -> key -> [(key, a)]
delFromAL l key = filter (\a -> (fst a) /= key) l

{- | Flips an association list.  Converts (key1, val), (key2, val) pairs
to (val, [key1, key2]). -}
flipAL :: (Eq key, Eq val) => [(key, val)] -> [(val, [key])]
flipAL oldl =
    let worker :: (Eq key, Eq val) => [(key, val)] -> [(val, [key])] -> [(val, [key])]
        worker [] accum = accum
        worker ((k, v):xs) accum =
            case lookup v accum of
                                Nothing -> worker xs ((v, [k]) : accum)
                                Just y -> worker xs (addToAL accum v (k:y))
        in
        worker oldl []

{- FIXME TODO: sub -}

{- | Returns a count of the number of times the given element occured in the
given list. -}
countElem :: Eq a => a -> [a] -> Int
countElem i l = length (elemIndices i l)

{- | Returns the rightmost index of the given element in the
given list. -}
elemRIndex :: Eq a => a -> [a] -> Maybe Int
elemRIndex item l =
    case reverse $ elemIndices item l of
                                   [] -> Nothing
                                   (x:_) -> Just x
{- | Like elemRIndex, but returns -1 if there is nothing
found. -}
alwaysElemRIndex :: Eq a => a -> [a] -> Int
alwaysElemRIndex item list =
    case elemRIndex item list of
                              Nothing -> -1
                              Just x -> x
