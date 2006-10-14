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

data SizeOpts = SizeOpts { base :: Integer,
                           powerIncr :: Integer,
                           firstPower :: Integer,
                           suffixes :: String}
                           
binaryOpts = SizeOpts {base = 2,
                       firstPower = 0,
                       suffixes = " KMGTPEZY",
                       powerIncr = 10}

siOpts = SizeOpts {base = 10,
                   firstPower = -24,
                   suffixes = "yzafpnum kMGTPEZY",
                   powerIncr = 3}

renderNum opts 0.0 = (0, snd $ renderNum opts 1)
renderNum opts number =
    (retnum, suffix)
    where incrList = map idx2pwr [0..genericLength (suffixes opts) - 1]
          incrIdxList = zip incrList [0..]
          idx2pwr i = i * powerIncr opts + firstPower opts
          
          (usedexp, expidx) =
              case find 
                  (\(x, _) -> (fromIntegral $ base opts) ** (fromIntegral x) <= number) 
                  (reverse incrIdxList) of
                  Just x -> x
                  Nothing -> last incrIdxList
          suffix = (suffixes opts !! (fromIntegral expidx))
          retnum = number / ((fromIntegral (base opts) ** (fromIntegral usedexp)))