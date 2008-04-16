{-
Copyright (C) 2008 John Goerzen <jgoerzen@complete.org>

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
   Module     : Data.BinPacking
   Copyright  : Copyright (C) 2008 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable

Tools for packing into bins

Written by John Goerzen, jgoerzen\@complete.org

This module is designed to solve this type of problem: Given a bunch of
objects of varying sizes, what is the best possible way to pack them into
fixed-size bins?  This can be used, for instance, by the datapacker program
to pack files onto CDs or DVDs; by manufacturing environments to pack
physical items into physicl bins; etc.

A description of bin packing algorithms can be found at
<http://en.wikipedia.org/wiki/Bin_packing_problem>.
-}

module Data.BinPacking (BinPacker,
                        BinPackerError(..),
                        packByOrder,
                        packLargeFirst
                       )

where
import Data.List
import Control.Monad.Error

{- | Potential errors returned as Left values by 'BinPacker' functions. 
Calling 'show' on this value will produce a nice error message suitable for
display. -}
data (Num size, Ord size, Show size, Show obj) => BinPackerError size obj = 
    BPTooFewBins [(size, obj)]                -- ^ Ran out of bins; attached value is the list of objects that don't fit
    | BPSizeTooLarge size (size, obj)   -- ^ Bin size1 exceeded by at least the given object and size
    | BPOther String                    -- ^ Other error
      deriving (Eq, Read)

instance (Num size, Ord size, Show size, Show obj) => Show (BinPackerError size obj) where
    show (BPTooFewBins _) = "Too few bins"
    show (BPSizeTooLarge binsize (objsize, obj)) =
        "Size " ++ show objsize ++ " greater than bin size " ++ show binsize
        ++ " at " ++ show obj
    show (BPOther x) = x

{- | Let us use this as part of the Either monad -}
instance (Num size, Ord size, Show size, Show obj) => Error (BinPackerError size obj) where
    strMsg = BPOther

{- | The primary type for bin-packing functions.

These functions take a list of size of bins.  If every bin is the same size,
you can pass @(repeat binSize)@ to pass an infinite list of bins if the
same size.  Any surplus bins will simply be ignored. -}
type BinPacker = (Num size, Ord size, Show size, Show obj) => 
                  [size]        -- ^ The sizes of bins
               -> [(size, obj)] -- ^ The sizes and objects
               -> Either (BinPackerError size obj) [[(size, obj)]] -- ^ Either error or results


{- | Pack objects into bins, preserving order.  Objects will be taken from the
input list one by one, and added to each bin until the bin is full.  Work will
then proceed on the next bin.  No attempt is made to optimize allocations to
bins.  This is the simplest and most naive bin-packing algorithm, but
may not make very good use of bin space. -}
packByOrder :: BinPacker
packByOrder _ [] = Right []                     -- Ran out of sizes
packByOrder [] remainder = Left (BPTooFewBins remainder)
packByOrder (thisbinsize:otherbins) sizes =
    let fillBin _ [] = Right []
        fillBin accumsize ((s, o):xs) 
            | s > thisbinsize = Left $ BPSizeTooLarge thisbinsize (s, o)
            | s + accumsize > thisbinsize = Right []
            | otherwise = do next <- fillBin (accumsize + s) xs
                             return $ (s, o) : next
        in do thisset <- fillBin 0 sizes
              next <- packByOrder otherbins (drop (length thisset) sizes)
              return (thisset : next)

{- | Pack objects into bins.  For each bin, start with the largest objects,
and keep packing the largest object from the remainder until no object can
be found to put in the bin.  This is substantially more efficient than
'packByOrder', but requires sorting the input. -}
packLargeFirst :: BinPacker
packLargeFirst bins sizes = packLargeFirst' bins (sortBy fstSort sizes)
    where fstSort a b = compare (fst a) (fst b)

packLargeFirst' :: BinPacker
packLargeFirst' _ [] = Right []                     -- Ran out of sizes
packLargeFirst' [] remainder = Left (BPTooFewBins remainder)
packLargeFirst' (thisbinsize:otherbins) sizes =
    let fillBin _ [] = Right []
        fillBin accumsize sizelist =
            case break (\x -> (fst x) + accumsize < thisbinsize) sizelist of
              (_, []) ->
                  if accumsize == 0
                     then Left $ BPSizeTooLarge thisbinsize (head sizelist)
                     else Right []
              (nonmatches, ((s, o):matchxs)) ->
                  do next <- fillBin (accumsize + s) (nonmatches ++ matchxs)
                     return $ (s, o) : next
        in do thisset <- fillBin 0 sizes
              next <- packLargeFirst' otherbins (drop (length thisset) sizes)
              return (thisset : next)
