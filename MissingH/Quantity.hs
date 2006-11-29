{-
Copyright (C) 2006 John Goerzen <jgoerzen@complete.org>

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
   Module     : MissingH.Quantity
   Copyright  : Copyright (C) 2006 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable

Tools for rendering sizes

Written by John Goerzen, jgoerzen\@complete.org -}

module MissingH.Quantity (
                          renderNum,
                          quantifyNum,
                          SizeOpts(..),
                          binaryOpts,
                          siOpts
                     )

where
import Data.List
import Text.Printf

{- | The options for 'quantifyNum' and 'renderNum' -}
data SizeOpts = SizeOpts { base :: Int, -- ^ The base from which calculations are made
                           powerIncr :: Int, -- ^ The increment to the power for each new suffix
                           firstPower :: Int, -- ^ The first power for which suffixes are given
                           suffixes :: String -- ^ The suffixes themselves
                         }
                           
{- | Predefined definitions for byte measurement in groups of 1024, from 0 to
2**80 -}
binaryOpts :: SizeOpts
binaryOpts = SizeOpts {base = 2,
                       firstPower = 0,
                       suffixes = " KMGTPEZY",
                       powerIncr = 10}

{- | Predefined definitions for SI measurement, from 10**-24 to 10**24. -}
siOpts :: SizeOpts
siOpts = SizeOpts {base = 10,
                   firstPower = -24,
                   suffixes = "yzafpnum kMGTPEZY",
                   powerIncr = 3}

{- | Takes a number and returns a new (quantity, suffix) combination.
The space character is used as the suffix for items around 0. -}
quantifyNum :: (Ord a, Real a, Floating b, Ord b) => SizeOpts -> a -> (b, Char)
quantifyNum opts n = (\(x, s) -> (head x, s)) $ quantifyNums opts [n]

{- | Like 'quantifyNum', but takes a list of numbers.  The first number in
the list will be evaluated for the suffix.  The same suffix and scale will
be used for the remaining items in the list.  Please see 'renderNums' for
an example of how this works.

It is invalid to use this function on an empty list. -}
quantifyNums :: (Ord a, Real a, Floating b, Ord b) => SizeOpts -> [a] -> ([b], Char)
quantifyNums _ [] = error "Attempt to use quantifyNums on an empty list"
quantifyNums opts (headnum:xs) =
    (map (\n -> procnum n) (headnum:xs), suffix)
    where number = case fromRational . toRational $ headnum of
                     0 -> 1
                     x -> x
          incrList = map idx2pwr [0..length (suffixes opts) - 1]
          incrIdxList = zip incrList [0..]
          idx2pwr i = i * powerIncr opts + firstPower opts
          finderfunc (x, _) = (fromIntegral $ base opts) ** (fromIntegral x) 
                              <= (abs number)
          -- Find the largest item that does not exceed the number given.
          -- If the number is larger than the larger item in the list,
          -- that's fine; we'll just write it in terms of what we have.

          (usedexp, expidx) =
              case find finderfunc (reverse incrIdxList) of
                  Just x -> x
                  Nothing -> head incrIdxList -- If not found, it's smaller than the first
          suffix = (suffixes opts !! (fromIntegral expidx))
          procnum n = (fromRational . toRational $ n) /
                      ((fromIntegral (base opts) ** (fromIntegral usedexp)))
          --(posres, possuf) = quantifyNum opts (headnum * (-1))

{- | Render a number into a string, based on the given quantities.  This is
useful for displaying quantities in terms of bytes or in SI units.  Give this
function the 'SizeOpts' for the desired output, and a precision (number of
digits to the right of the decimal point), and you get a string output.

Here are some examples:

> MissingH.Quantity> renderNum binaryOpts 0 1048576
> "1M"
> MissingH.Quantity> renderNum binaryOpts 2 10485760
> "10.00M"
> MissingH.Quantity> renderNum binaryOpts 3 1048576
> "1.000M"
> MissingH.Quantity> renderNum binaryOpts 3 1500000
> "1.431M"
> MissingH.Quantity> renderNum binaryOpts 2 (1500 ** 3)
> "3.14G"

> MissingH.Quantity> renderNum siOpts 2 1024
> "1.02k"
> MissingH.Quantity> renderNum siOpts 2 1048576
> "1.05M"
> MissingH.Quantity> renderNum siOpts 2 0.001
> "1.00m"
> MissingH.Quantity> renderNum siOpts 2 0.0001
> "100.00u"

If you want more control over the output, see 'quantifyNum'. -}
renderNum :: (Ord a, Real a) => 
             SizeOpts
          -> Int                -- ^ Precision of the result
          -> a                  -- ^ The number to examine
          -> String
renderNum opts prec number =
    (printf ("%." ++ show prec ++ "g") num) ++ [suffix]
    where (num, suffix) = (quantifyNum opts number)::(Double, Char)

{- | Like 'renderNum', but operates on a list of numbers.  The first number
in the list will be evaluated for the suffix.  The same suffix and scale will
be used for the remaining items in the list.  See 'renderNum' for more
examples.

Also, unlike 'renderNum', the %f instead of %g printf format is used so that
\"scientific\" notation is avoided in the output.

Examples:

> *MissingH.Quantity> renderNums binaryOpts 3 [1500000, 10240, 104857600]
> ["1.431M","0.010M","100.000M"]
> *MissingH.Quantity> renderNums binaryOpts 3 [1500, 10240, 104857600]
> ["1.465K","10.000K","102400.000K"]

-}
renderNums :: (Ord a, Real a) =>
              SizeOpts
           -> Int               -- ^ Prevision of the result
           -> [a]               -- ^ The numbers to examine
           -> [String]          -- ^ Result
renderNums opts prec numbers =
    map printit convnums
    where printit num =
              (printf ("%." ++ show prec ++ "f") num) ++ [suffix]
          (convnums, suffix) = 
              (quantifyNums opts numbers)::([Double], Char)
