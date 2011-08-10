{- arch-tag: Tuple utilities main file
Copyright (c) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : Data.Tuple.Utils
   Copyright  : Copyright (C) 2004-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable

This module provides various helpful utilities for dealing with lists.

Written by Neil Mitchell, <http://www.cs.york.ac.uk/~ndm/>
-}

module Data.Tuple.Utils(
    -- * Extraction
    fst3, snd3, thd3
    ) where


-- | Take the first item out of a 3 element tuple
fst3 :: (a,b,c) -> a
fst3 (a,b,c) = a

-- | Take the second item out of a 3 element tuple
snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b

-- | Take the third item out of a 3 element tuple
thd3 :: (a,b,c) -> c
thd3 (a,b,c) = c
