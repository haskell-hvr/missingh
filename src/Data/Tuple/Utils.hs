{-# LANGUAGE Safe #-}
{- arch-tag: Tuple utilities main file
Copyright (c) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : Data.Tuple.Utils
   Copyright  : Copyright (C) 2004-2011 John Goerzen
   SPDX-License-Identifier: BSD-3-Clause

   Stability  : stable
   Portability: portable

This module provides various helpful utilities for dealing with tuples.

Written by Neil Mitchell, <http://www.cs.york.ac.uk/~ndm/>
-}

module Data.Tuple.Utils(
    -- * Construction
    dup, triple,
    -- * Extraction
    fst3, snd3, thd3
    ) where

-- | Construct a pair by duplication of a single value
--
-- @since 1.4.3.0
dup :: a -> (a,a)
dup a = (a,a)

-- | Construct a 3-tuple from a single value
--
-- @since 1.4.3.0
triple :: a -> (a,a,a)
triple a = (a,a,a)

-- | Take the first item out of a 3 element tuple
fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

-- | Take the second item out of a 3 element tuple
snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

-- | Take the third item out of a 3 element tuple
thd3 :: (a,b,c) -> c
thd3 (_,_,c) = c
