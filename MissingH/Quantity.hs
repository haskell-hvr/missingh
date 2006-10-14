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

module MissingH.Quantity (quantifyNum,
                          renderNum,
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
binaryOpts = SizeOpts {base = 2,
                       firstPower = 0,
                       suffixes = " KMGTPEZY",
                       powerIncr = 10}

{- | Predefined definitions for SI measurement, from 10**-24 to 10**24. -}
siOpts = SizeOpts {base = 10,
                   firstPower = -24,
                   suffixes = "yzafpnum kMGTPEZY",
                   powerIncr = 3}

{- | Takes a number and returns a new (quantity, suffix) combination.
The space character is used as the suffix for items around 0. -}
quantifyNum :: (Ord a, Real a, Floating b, Ord b) => SizeOpts -> a -> (b, Char)
quantifyNum opts 0 = (0, snd $ quantifyNum opts 1)
quantifyNum opts inpnumber 
    | inpnumber < 0 = 
        (posres * (-1), possuf)
    | otherwise = (retnum, suffix)
    where number = fromRational . toRational $ inpnumber
          incrList = map idx2pwr [0..length (suffixes opts) - 1]
          incrIdxList = zip incrList [0..]
          idx2pwr i = i * powerIncr opts + firstPower opts
          finderfunc (x, _) = (fromIntegral $ base opts) ** (fromIntegral x) 
                              <= number
          -- Find the largest item that does not exceed the number given.
          -- If the number is larger than the larger item in the list,
          -- that's fine; we'll just write it in terms of what we have.

          (usedexp, expidx) =
              case find finderfunc (reverse incrIdxList) of
                  Just x -> x
                  Nothing -> head incrIdxList -- If not found, it's smaller than the first
          suffix = (suffixes opts !! (fromIntegral expidx))
          retnum = number / ((fromIntegral (base opts) ** (fromIntegral usedexp)))
          (posres, possuf) = quantifyNum opts (inpnumber * (-1))

renderNum :: (Ord a, Real a) => 
             SizeOpts
          -> Int                -- ^ Precision of the result
          -> a                  -- ^ The number to examine
          -> String
renderNum opts prec number =
    (printf ("%." ++ show prec ++ "g") num) ++ [suffix]
    where (num, suffix) = (quantifyNum opts number)::(Double, Char)