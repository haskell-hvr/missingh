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

Copyright (c) 2004 John Goerzen, jgoerzen\@complete.org

Some code in sub-modules written by Ian Lynagh

Inspiration and ideas from haskell-xml-rpc by Bjorn Bringert

Please scroll down to read the detailed documentation.

-}

module MissingH.Printf(-- * Introduction
                       -- $introduction

                       -- * Methods of Use
                       -- $methodsofuse

                       -- ** Variable-Argument Ouptut
                       vsprintf,
                       vprintf,
                       vfprintf,
                       -- *** Casting Notes
                       -- $castingnotes
                       ps, pio,

                       -- ** List-Argument Output
                       sprintf,
                       printf,
                       fprintf,
                       -- ** Utility Function
                       v,
                       -- * Differences from C
                       -- $differencesfromc

                       -- * Important Haskell Notes
                       -- $haskellnotes

                       -- * Full Example Programs
                       -- $fullexamples

                       -- * Underlying Types
                       Value(..),
                       PFRun,
                       PFType(..),
                       IOPFRun

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

--type LookupFunc a :: String -> a -> (String, String, a)
normLookup :: String -> [Value] -> (String, String, [Value])
normLookup xs (y : ys) =
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
                 (fix_width flags width ((get_conversion_func fmt y flags width prec)), remainder, ys)
         _ -> error $ "Problem matching format string at %" ++ xs
    
{- | List version of 'vsprintf'. -}
sprintf :: String -> [Value] -> String
sprintf [] [] = []
sprintf ('%' : '%' : xs) y = '%' : sprintf xs y
sprintf ('%' : xs) y =
    let (this, remainder, ys) = normLookup xs y
        in
        this ++ sprintf remainder ys
sprintf (x:xs) y = x : sprintf xs y

{-
sprintf :: String -> [Value] -> String
sprintf = sprintfG id sprintf
-}

{-
{- | Association list printing -}
sprintfAL :: String -> PrintfAL -> String
sprintfAL [] _ = []
sprintfAL ('%' : '%' : xs) y = '%' : sprintfAL xs y
sprintfAL ('%' : xs) (y : ys) =
-}
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

{- | Utility to force something to a string -}
ps :: String -> String
ps = id

{- | Utility to force something to an IO () -}
pio :: IO () -> IO ()
pio = id

----------------------------------------------------------------------
-- Documentation for this module
----------------------------------------------------------------------

{- $introduction
Welcome to the Haskell printf support.  This module is designed to emulate the
C printf(3) family of functions.  Here are some quick introductory examples:

#examples#

>vsprintf "Hello"
>> "Hello"
>vsprintf "Hello, %s\n" "John"
>> "Hello, John\n"
>vsprintf "%s, your age is %d\n" "John" (10::Integer)
>> "John, your age is 10\n"

Or, using the list-passing method:

>sprintf "Hello" []
>> "Hello"
>sprintf "Hello, %s\n" [v "John"]
>> "Hello, John\n"
>sprintf "%s, your age is %d\n" [v "John", v (10::Integer)]
>> "John, your age is 10\n"

You can also work with I\/O with these:

>main :: IO ()
>main = do
>       pio $ vprintf "Line1\n"
>       pio $ vprintf "Line2: %s\n" "blah"
>       vprintf "Line3: done\n"

This will print @Line1\\nLine2: blah\\nLine3: done\\n@ to standard output.
You can also use the list form:

>main :: IO ()
>main = do
>       printf "Line1\n" []
>       printf "Line2: %s\n" [v "blah"]
>       printf "Line3: done\n" []

-}

{- $methodsofuse
As you can see, there are two different ways to access the printf functions:
via the variable argument count support (the functions beginning with v)
or via the list argument support.  There is a utility function, 'v', that
is simply a shortcut for 'toValue'.
-}

{- $castingnotes
If you are running in an interactive situation, or something where the
compiler cannot deduce the expected return type, you will need to cast it
to @String@.  For instance, at the ghci prompt, you would have to say
@(sprintf \"foo\")::String@ to make things work.  If you are using one of the
I\/O variants, you will have to instead cast it to @IO ()@.

To make this easier, there are two functions: 'ps' and 'pio'.  They
simply provide an easy idiom to force things to the proper type.  Examples:

>main :: IO ()
>main = do
>       pio $ vprintf "Line1\n"
>       pio $ vprintf "Line2: %s\n" "blah"
>       vprintf "Line3: done\n"

Note that in this case, no 'pio' was necessary for the third line.
That's because @main@ was declared to return @IO ()@ already, so the type
system knows what to do.  If that declaration was missing, the 'pio'
would have been required there as well.

These special cases apply only to the \"v\" functions.
-}
   

{- $differencesfromc
These functions are very similar to the C functions.  Here is a list of the
known differences:

* There is a new conversion type %H.  This will take any type of data
already converted to a value and display it in its native representation from
show.  This may not be what you expect for integers, and is likely to be
altered in the future, so use with caution.

* There is no support for the length specifiers (l, ll, etc.) since Haskell's
type system provides all the information we need.

* There is no support for the %n, %*, %\$ forms that the C printf() supports.
These make less sense in Haskell.

-}

{- $haskellnotes
Please be aware of the following as you use this module:

If the type system cannot determine the type of an argument (as in the
numeric literals in the examples at "MissingH.Printf#examples"), you may have to explicitly cast it to something.
In practice, this is only a problem in interactive situations like ghci or
hugs.

Floating-point values are converted to a Double for display.  If you are
using some floating-point value with an extremely high precision (such
as a Rational), be aware that some of this precision may be lost for display.x

When run with Hugs, you must use @-98 +o@ on your command line.

-}

{- $fullexamples

Here are some full example programs.  You can compile and run these directly.

This example acts as a filter that adds a line number and length to each
line from input:

>import MissingH.Printf
>
>convlines :: Int -> [String] -> [String]
>convlines _ [] = []
>convlines count (line:xs) =
>    vsprintf "%6d, len %03d: %s" count (length line) line : 
>            convlines (count + 1) xs
>
>main = interact $ unlines . convlines 1 . lines

If you have a sample file like this:

>Hello,
>
>This is a test.
>Haskell is really neat.

Then running @.\/test < file.txt@ will produce:

>     1, len 006: Hello,
>     2, len 000:
>     3, len 015: This is a test.
>     4, len 023: Haskell is really neat.

And so on -- and everything will be nicely lined up since the line numbers
will grow to the left.

Here's another example of a little bit of interaction:

>import MissingH.Printf
>import System.IO
>
>main = do
>       hSetBuffering stdout NoBuffering
>       printf "Welcome.  Please enter your name: " []
>       name <- getLine
>       printf "Hello, %s.  Please enter your age: " [v name]
>       agestr <- getLine
>       let age = (read agestr)::Int
>       printf "%s, you are at least %d months old.\n" [v name, v $ age * 12]

Here's a sample session:

>Welcome.  Please enter your name: Bill
>Hello, Bill.  Please enter your age: 53
>Bill, you are at least 636 months old.

The printf functions are also great for creating reports nicely lined up
by column.  Here's an example:

>import MissingH.Printf
>import MissingH.IO
>import Data.List
>
>fmt = "%-10d %010d %010X"
>
>fmtlines :: Int -> [String] -> [String]
>fmtlines _ [] = []
>fmtlines count (x:xs) =
>    let l = length x in
>        vsprintf fmt count l l : fmtlines (count + 1) xs
>
>main = do
>       pio $ vprintf ("%-10s %-10s %s\n") "Line #" "Length Dec" "Length Hex"
>       putStrLn $ (replicate 10 '-') ++ " " ++ (replicate 10 '-') ++
>                " " ++ (replicate 10 '-')
>       lineInteract $ fmtlines 1

When applied to the same example file as before, the output will be:

>Line #     Length Dec Length Hex
>---------- ---------- ----------
>1          0000000006 0000000006
>2          0000000000 0000000000
>3          0000000015 000000000F
>4          0000000023 0000000017

-}