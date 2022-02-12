{-# LANGUAGE Safe #-}
{- arch-tag: Maybe utilities
Copyright (c) 2005-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : Data.Maybe.Utils
   Copyright  : Copyright (C) 2005-2011 John Goerzen
   SPDX-License-Identifier: BSD-3-Clause

   Stability  : stable
   Portability: portable

Utilities for working with the 'Maybe' data type.

-}
module Data.Maybe.Utils
    (
     forceMaybe, forceMaybeMsg
) where

import Data.Maybe (fromJust)

{- | Pulls a 'Just' value out of a 'Maybe' value.  If the 'Maybe' value is
'Nothing', raises an exception with error.

Alias of 'Data.Maybe.fromJust'. -}
forceMaybe :: Maybe a -> a
forceMaybe = fromJust

{- | Like 'forceMaybe', but lets you customize the error message raised if
'Nothing' is supplied. -}
forceMaybeMsg :: String -> Maybe a -> a
forceMaybeMsg msg = maybe (error msg) id
