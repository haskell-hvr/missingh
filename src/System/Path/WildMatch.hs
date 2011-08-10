{-
Copyright (c) 2006-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : System.Path.WildMatch
   Copyright  : Copyright (C) 2006-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Matching filenames with wildcards.  See also "System.Path.Glob" for
support for generating lists of files based on wildcards.

Inspired by fnmatch.py, part of the Python standard library.

Written by John Goerzen, jgoerzen\@complete.org

The input wildcard for functions in this module is expected to be in
the standard style of Posix shells.

That is:

>? matches exactly one character
>\* matches zero or more characters
>[list] matches any character in list
>[!list] matches any character not in the list

The returned regular expression will always end in \$ but never begins
with ^, making it suitable for appending to the end of paths.  If you want to
match a given filename directly, you should prepend the ^ character to the
returned value from this function.

Please note:

* Neither the path separator (the slash or backslash) nor the period carry
any special meaning for the functions in this module.  That is, @*@ will
match @\/@ in a filename.  If this is not the behavior you want, you probably
want "System.Path.Glob" instead of this module.

* Unlike the Unix shell, filenames that begin with a period are not ignored
by this module.  That is, @*.txt@ will match @.test.txt@.

* This module does not current permit escaping of special characters.
-}

module System.Path.WildMatch (-- * Wildcard matching
                                wildCheckCase,
                                wildToRegex)
    where

import Text.Regex
import Data.String.Utils

{- | Convert a wildcard to an (uncompiled) regular expression.

-}
wildToRegex :: String -> String
wildToRegex i = convwild i ++ "$"

{- | Check the given name against the given pattern, being case-sensitive.

The given pattern is forced to match the given name starting at the beginning.
 -}
wildCheckCase :: String         -- ^ The wildcard pattern to use as the base
              -> String         -- ^ The filename to check against it
              -> Bool           -- ^ Result
wildCheckCase patt name =
    case matchRegex (mkRegex $ "^" ++ wildToRegex patt) name of
      Nothing -> False
      Just _ -> True

-- This is SO MUCH CLEANER than the python implementation!
convwild :: String -> String
convwild [] = []
convwild ('*':xs) = ".*" ++ convwild xs
convwild ('?':xs) = "." ++ convwild xs
convwild ('[':'!':xs) = "[^" ++ convpat xs
convwild ('[':xs) = '[' : convpat xs
convwild ('.':xs) = "\\." ++ convwild xs
convwild (x:xs) = escapeRe [x] ++ convwild xs

convpat :: String -> String
convpat ('\\':xs) = "\\\\" ++ convpat xs
convpat (']':xs) = ']' : convwild xs
convpat (x:xs) = x : convpat xs
convpat [] = []
