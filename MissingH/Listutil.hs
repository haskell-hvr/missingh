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
                         startswith, endswith
                        ) where

{- | Returns true if the given list starts with the specified elements;
false otherwise.

Example:

> startswith "He" "Hello" -> True

-}

startswith :: Eq a => [a] -> [a] -> Bool
startswith [] _ = True
startswith _ [] = False
startswith (x:xs) (l:ls) =
    if (x == l) then startswith xs ls
    else False

{- | Returns true if the given list ends with the specified elements;
false otherwise.

Example:

> endswith "lo" "Hello" -> True

-}
endswith :: Eq a => [a] -> [a] -> Bool
endswith x l = startswith (reverse x) (reverse l)
