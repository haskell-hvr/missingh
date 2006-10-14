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
   Module     : MissingH.Size
   Copyright  : Copyright (C) 2006 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable

Tools for rendering sizes

Written by John Goerzen, jgoerzen\@complete.org -}

module MissingH.Size (
                     )

where
import Data.List

data SizeOpts = SizeOpts { base :: Int,
                           powerIncr :: Int,
                           firstPower :: Int,
                           suffixes :: String}
                           
binaryOpts = SizeOpts {base = 2,
                       firstPower = 0,
                       suffixes = " KMGTPEZY",
                       powerIncr = 10}

siOpts = SizeOpts {base = 10,
                   firstPower = -24,
                   suffixes = "yzafpnum kMGTPEZY",
                   powerIncr = 3}

renderNum :: (Ord a, Real a, Floating b, Ord b) => SizeOpts -> a -> (b, Char)
renderNum opts 0 = (0, snd $ renderNum opts 1)
renderNum opts inpnumber 
    | inpnumber < 0 = 
        (posres * (-1), possuf)
    | otherwise = (retnum, suffix)
    where number = fromRational . toRational $ inpnumber
          incrList = map idx2pwr [0..length (suffixes opts) - 1]
          incrIdxList = zip incrList [0..]
          idx2pwr i = i * powerIncr opts + firstPower opts
          finderfunc (x, _) = (fromIntegral $ base opts) ** (fromIntegral x) 
                              <= number
          (usedexp, expidx) =
              case find finderfunc (reverse incrIdxList) of
                  Just x -> x
                  Nothing -> head incrIdxList -- If not found, it's smaller than the first
          suffix = (suffixes opts !! (fromIntegral expidx))
          retnum = number / ((fromIntegral (base opts) ** (fromIntegral usedexp)))
          (posres, possuf) = renderNum opts (inpnumber * (-1))