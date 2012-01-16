{- arch-tag: String utilities main file
Copyright (c) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : Data.String.Utils
   Copyright  : Copyright (C) 2004-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

This module provides various helpful utilities for dealing with strings.

Written by John Goerzen, jgoerzen\@complete.org
-}

module Data.String.Utils
                       (-- * Whitespace Removal
                        strip, lstrip, rstrip,
                        -- * Tests
                        -- | Note: These functions are aliases for functions
                        -- in "Data.List.Utils".
                        startswith, endswith,
                        -- * Conversions
                        -- | Note: Some of these functions are aliases for functions
                        -- in "Data.List.Utils".
                        join, split, splitWs, replace, escapeRe,
                        -- * Reading
                        maybeRead
                       ) where

import Data.List.Utils (startswith, endswith, join, split, replace)
import Data.Char (isAlpha, isAscii, isDigit)
import Data.Maybe (listToMaybe)
import Text.Regex (mkRegex, splitRegex)

wschars :: String
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

-- | Attempts to parse a value from the front of the string.
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
