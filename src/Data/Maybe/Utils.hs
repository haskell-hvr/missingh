{- arch-tag: Maybe utilities
Copyright (c) 2005-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : Data.Maybe.Utils
   Copyright  : Copyright (C) 2005-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable

Utilities for working with the Either data type

-}
module Data.Maybe.Utils
    (
     forceMaybe, forceMaybeMsg
) where

{- | Pulls a Just value out of a Maybe value.  If the Maybe value is
Nothing, raises an exception with error. -}
forceMaybe :: Maybe a -> a
forceMaybe = forceMaybeMsg "forceMaybe: Got Nothing"

{- | Like 'forceMaybe', but lets you customize the error message raised if
Nothing is supplied. -}
forceMaybeMsg :: String -> Maybe a -> a
forceMaybeMsg msg Nothing = error msg
forceMaybeMsg _ (Just x) = x
