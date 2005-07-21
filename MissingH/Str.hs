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

{- |
   Module     : MissingH.Str
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

This module provides various helpful utilities for dealing with strings.

Written by John Goerzen, jgoerzen\@complete.org
-}

module MissingH.Str(-- * Whitespace Removal
                        strip, lstrip, rstrip,
                        -- * Tests
                        -- | Note: These functions are aliases for functions
                        -- in "MissingH.List".
                        startswith, endswith,
                        -- * Conversions
                        -- | Note: Some of these functions are aliases for functions
                        -- in "MissingH.List".
                        join, split, splitRe, replace, subRe
                       ) where
import MissingH.List(startswith, endswith, join, split, replace)
import Text.Regex

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

-}
subRe :: Regex                          -- ^ Search pattern
      -> String                         -- ^ Input string
      -> String                         -- ^ Replacement text
      -> String                         -- ^ Output string
subRe _ "" _ = ""
subRe regexp inp repl = 
    let bre = mkRegex "\\\\(\\\\|[0-9]+)"
        lookup _ [] _ = []
        lookup [] _ _ = []
        lookup match repl groups =
            case matchRegexAll bre repl of
                Nothing -> repl
                Just (lead, _, trail, bgroups) ->
                    let newval = if (head bgroups) == "\\"
                                 then "\\"
                                 else let index = (read (head bgroups)) - 1
                                          in
                                          if index == -1
                                             then match
                                             else groups !! index
                        in
                        lead ++ newval ++ lookup match trail groups
        in
        case matchRegexAll regexp inp of
            Nothing -> inp
            Just (lead, match, trail, groups) ->
              lead ++ lookup match repl groups ++ (subRe regexp trail repl)

{- | Splits a string based on a regular expression.  The regular expression
should identify one delimiter.
-}

splitRe :: Regex -> String -> [String]
splitRe _ [] = []
splitRe delim str =
    case matchRegexAll delim str of
       Nothing -> [str]
       Just (firstline, _, remainder, _) ->
           if remainder == ""
              then firstline : [] : []
              else firstline : splitRe delim remainder

{-

splitRe :: Regex -> String -> [String]
splitRe delim [] = []
splitRe delim str =
    case matchRegexAll delim str of
        Just (prefix, match, postfix, subex) ->
            if (prefix == "") && (postfix == "") && (match == str) then
               -- We matched just a delimiter
               [] : [] : []
            else prefix : splitRe delim postfix
        Nothing -> [str]

-}
