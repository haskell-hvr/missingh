{- arch-tag: Printf utilities main file
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
   Module     : MissingH.Printf
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

This module provides various helpful utilities for using a C-style printf().

Written by John Goerzen, jgoerzen\@complete.org
-}

module MissingH.Printf(-- * Variable-Argument Ouptut
                       vsprintf,
                       vprintf,
                       vfprintf,
                       -- * List-Argument Output
                       sprintf,
                       printf,
                       fprintf,
                       -- * Utility Function
                       v,
                       -- * Underlying Types
                       Value(..),
                       PFRun,
                       PFType(..),
                       IOPFRun,
                       get_conversion_func
                       ) where

import MissingH.Str
import Data.List
import System.IO
import MissingH.Printf.Types
import MissingH.Printf.Printer(get_conversion_func)
import Text.Regex

v :: PFType a => a -> Value
v = toValue

sprintfre = mkRegex "^([#0 +'O-]*)([0-9]*)(\\.[0-9]*)?(.)"

toflags :: String -> [Flag]
toflags "" = []
toflags (x:xs) = (case x of
                      '#' -> AlternateForm
                      '0' -> ZeroPadded
                      '-' -> LeftAdjust
                      ' ' -> BlankPlus
                      '+' -> Plus
                      '\'' -> Thousands
                      'I' -> AlternativeDigits) : toflags xs

sprintf :: String -> [Value] -> String
sprintf [] [] = []
sprintf ('%' : '%' : xs) y = '%' : sprintf xs y
{-
sprintf ('%' : xs) (y : ys) = (fromValue y) ++ sprintf xs ys
sprintf ('!' : xs) (y : ys) = 
    show (((fromValue y)::Int) + 1) ++ sprintf xs ys -}

{-
sprintf ('%' : t : xs) (y:ys) = 
    let cv = get_conversion_func t y [] Nothing Nothing
        in
        cv ++ sprintf xs ys
-}

sprintf ('%' : xs) (y : ys) =
    case matchRegexAll sprintfre xs of
         Nothing -> error $ "Problem in format string at %" ++ xs
         --Just (_, _, r, x) -> "<" ++ show x ++ ">" ++ sprintf r ys
         Just (_, _, remainder, [flagstr, widthstr, precstr, [fmt]]) ->
             let width = if widthstr == "" then Nothing else Just (read widthstr)
                 prec = if precstr == "" then Nothing else Just precstr
                 flags = toflags flagstr
                 in
                 (get_conversion_func fmt y flags width prec) ++ sprintf remainder ys
         _ -> error $ "Problem matching format string at %" ++ xs

sprintf (x:xs) y = x : sprintf xs y

vsprintf :: (PFRun a) => String -> a
vsprintf f = pfrun $ sprintf f

fprintf :: Handle -> String -> [Value] -> IO ()
fprintf h f v = hPutStr h $ sprintf f v

printf :: String -> [Value] -> IO ()
printf f v = fprintf stdout f v

vfprintf :: IOPFRun a => Handle -> String -> a
vfprintf h f = iopfrun h $ sprintf f

vprintf :: IOPFRun a => String -> a
vprintf f = vfprintf stdout f
