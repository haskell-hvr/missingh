{- arch-tag: String utilities main file
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

{- | This module provides various helpful utilities for dealing with strings.

Written by John Goerzen, jgoerzen@complete.org
-}

module MissingH.Strutil(
                        -- * Whitespace Removal
                        strip, lstrip, rstrip) where

wschars = " \t\r\n"

{- | Removes any whitespace characters that are present at the start
or end of a string. Does not alter the internal contents of a
string. If no whitespace characters are present at the start or end
of a string, returns the original string unmodified. Safe to use on
any string.

Note that this may differ from some other similar
functions from other authors in that:

1. If multiple whitespace
characters are present all in a row, they are all removed;

2. If no
whitespace characters are present, nothing is done.
-}

strip :: String -> String
strip = lstrip . rstrip

-- | Same as 'strip', but applies only to the left side of the string.
lstrip :: String -> String
lstrip s = case s of
                  [] -> []
                  (x:xs) -> if elem x wschars
                            then lstrip xs
                            else s

-- | Same as 'strip', but applies only to the right side of the string.
rstrip :: String -> String
rstrip = reverse . lstrip . reverse

-- | Returns true if the given list starts with the specified elements;
-- false otherwise.
--
-- Example: startswith "He" "Hello" -> True

startswith :: Eq a => [a] -> [a] -> Bool
startswith [] _ = True
startswith _ [] = False
startswith (x:xs) (l:ls) =
    if (x == l) then startswith xs ls
    else False

-- | Returns true if the given list ends with the specified elements;
-- false otherwise.
--
-- Example: endswith "lo" "Hello" -> True
endswith :: Eq a => [a] -> [a] -> Bool
endswith x l = startswith (reverse x) (reverse l)
