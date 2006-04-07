{- arch-tag: String utilities main file
Copyright (C) 2004-2006 John Goerzen <jgoerzen@complete.org>

This program is free software; you can redistribute it and\/or modify
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
   Module     : MissingH.Str
   Copyright  : Copyright (C) 2004-2006 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

This module provides various helpful utilities for dealing with strings.

Written by John Goerzen, jgoerzen\@complete.org
-}

module MissingH.Str
                       (-- * Whitespace Removal
                        strip, lstrip, rstrip,
                        -- * Tests
                        -- | Note: These functions are aliases for functions
                        -- in "MissingH.List".
                        startswith, endswith,
                        -- * Conversions
                        -- | Note: Some of these functions are aliases for functions
                        -- in "MissingH.List".
                        join, split, splitWs, splitRe, replace, subRe, escapeRe
                       ) where
import MissingH.List(startswith, endswith, join, split, replace)
import Data.Char
import Text.Regex

{-# DEPRECATED splitRe "Use Text.Regex.splitRegex instead" #-}
{-# DEPRECATED subRe "Use Text.Regex.subRegex instead" #-}

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

{- | Replaces every occurance of the given regexp with the replacement string.

In the replacement string, @\"\\1\"@ refers to the first substring; 
@\"\\2\"@ to the second, etc; and @\"\\0\"@ to the entire match.
@\"\\\\\\\\\"@ will insert a literal backslash.

This code has been integrated into the standard Haskell libraries
as Text.Regex.subRegex and is thus deprecated in MissingH.
-}
subRe :: Regex                          -- ^ Search pattern
      -> String                         -- ^ Input string
      -> String                         -- ^ Replacement text
      -> String                         -- ^ Output string
subRe = subRegex

{- | Splits a string based on a regular expression.  The regular expression
should identify one delimiter.

This code has been integrated into the standard Haskell libraries
as Text.Regex.splitRegex and is thus deprecated in MissingH.
-}

splitRe :: Regex -> String -> [String]
splitRe = splitRegex

{- | Splits a string around whitespace.  Empty elements in the result
list are automatically removed. -}
splitWs :: String -> [String]
splitWs = filter (\x -> x /= []) . splitRegex (mkRegex "[ \t\n\r\v\f]+")

{- | Escape all characters in the input pattern that are not alphanumeric.

Does not make special allowances for NULL, which isn't valid in a
Haskell regular expression pattern. -}
escapeRe :: String -> String
escapeRe [] = []
escapeRe (x:xs)
    -- Chars that we never escape
    | x `elem` ['\'', '`'] = x : escapeRe xs
    -- General rules for chars we never escape
    | isDigit x || (isAscii x && isAlpha x) || x `elem` ['<', '>'] 
        = x : escapeRe xs
    -- Escape everything else
    | otherwise = '\\' : x : escapeRe xs
