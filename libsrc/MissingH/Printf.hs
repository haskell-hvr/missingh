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

Some code in sub-modules written by Ian Lynagh

Inspiration and ideas from haskell-xml-rpc by Bjorn Bringert

Welcome to the Haskell printf support.  This module is designed to emulate the
C printf(3) family of functions.  Here are some examples:


>> vsprintf "Hello"
> "Hello"
>> vsprintf "Hello, %s\n" "John"
> "Hello, John\n"
>> vsprintf "%s, your age is %d\n" "John" (10::Integer)
> "John, your age is 10\n"

Or, using the list-passing method:

>> sprintf "Hello" ([]::[Value])
> "Hello"
>> sprintf "Hello, %s\n" [v "John"]
> "Hello, John\n"
>> sprintf "%s, your age is %d\n" [v "John", v (10::Integer)]
> "John, your age is 10\n"

You can also work with I\/O with these:

> let printer = do
>               printf "Line1\n"
>               printf "Line2: %s\n" "blah"

This will print @Line1\\nLine2: blah\\n@ to standard output.

As you can see, there are two different ways to access the printf functions:
via the variable argument count support (the functions beginning with v)
or via the list argument support.  There is a utility function, 'v', that
is simply a shortcut for 'toValue'.

These functions are very similar to the C functions, with the following caveats:

* There is no support for the length specifiers (l, etc.) since these make no
sense in Haskell.  Haskell's type system provides all the info we need.

* If the type system cannot determine the type if an argument (as in the
numeric literals above), you may have to explicitly cast it to something.
In practice, this is only a problem in interactive situations like ghci or
hugs.

* If you are running in an interact situation, or something where the
compiler cannot deduce the expected return type, you will need to cast it
to @String@.  For instance, at the ghci prompt, you would have to say
@(sprintf \"foo\")::String@ to make things work.  If you are using one of the
I\/O variants, you will have to instead cast it to @IO ()@.

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
import MissingH.Printf.Printer(get_conversion_func, fix_width)
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

mkflags :: String -> [Flag]
mkflags x =
    let flags = toflags x
        flags' = if LeftAdjust `elem` flags then filter (/= ZeroPadded) flags
                                            else flags
        flags'' = if Plus `elem` flags then filter (/= BlankPlus) flags
                                       else flags'
        in
        flags''

{- | List version of 'vsprintf'. -}

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
             let width = if widthstr == "" then Nothing else Just ((read widthstr)::Width)
                 prec = if precstr == "" 
                        then Nothing 
                        else Just (if length precstr >= 1
                                   then abs $ read (drop 1 precstr)
                                   else 0)
                 flags = mkflags flagstr
                 in
                 --(show width) ++ sprintf remainder ys
                 fix_width flags width ((get_conversion_func fmt y flags width prec)) ++ sprintf remainder ys
         _ -> error $ "Problem matching format string at %" ++ xs

sprintf (x:xs) y = x : sprintf xs y

{- | Given a format string and zero or more arguments, return a string
that has formatted them appropriately.  This is the variable argument version
of 'sprintf'. -}
vsprintf :: (PFRun a) => String -> a
vsprintf f = pfrun $ sprintf f

{- | Like 'sprintf', but instead of returning a string, directs output
to the given Handle. -}
fprintf :: Handle -> String -> [Value] -> IO ()
fprintf h f v = hPutStr h $ sprintf f v

{- | Like 'fprintf', but directs output to standard out instead of
taking an explicit Handle. -}
printf :: String -> [Value] -> IO ()
printf f v = fprintf stdout f v

{- | Like 'vsprintf', but instead of returning a string, directs output to
the given Handle. -}
vfprintf :: IOPFRun a => Handle -> String -> a
vfprintf h f = iopfrun h $ sprintf f

{- | Like 'vfprintf', but directs output to standard out instead of taking
an explicit Handle. -}
vprintf :: IOPFRun a => String -> a
vprintf f = vfprintf stdout f
