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

{- | This module provides various helpful utilities for dealing with lists.

Written by John Goerzen, jgoerzen\@complete.org
-}

module MissingH.Listutil(-- * Tests
                         startswith, endswith,
                         -- * Conversions
                         split, join, trunc
                        ) where
import Data.List(intersperse, concat, isPrefixOf, isSuffixOf)

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

{- | Given a length and a list, remove any elements at the end of the list
that make it longer than the length.  If the list is shorter than the
length, do nothing.

Example:

> trunc 2 "Hello" -> "He"
-}

trunc :: Int -> [a] -> [a]
trunc maxlen list =
    let removecount = (length list) - maxlen in
        if (removecount < 1) then list
        else reverse $ drop removecount $ reverse list
