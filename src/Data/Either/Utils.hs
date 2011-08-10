{- arch-tag: Euither utilities
Copyright (c) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : Data.Either.Utils
   Copyright  : Copyright (C) 2004-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable

Utilities for working with the Either data type

-}
module Data.Either.Utils
    (
     maybeToEither,
     forceEither,
     forceEitherMsg,
     eitherToMonadError,
     fromLeft, fromRight, fromEither
) where
import Control.Monad.Error

{- | Converts a Maybe value to an Either value, using the supplied parameter
as the Left value if the Maybe is Nothing.

This function can be interpreted as:

@maybeToEither :: e -> Maybe a -> Either e a@

Its definition is given as it is so that it can be used in the Error and related monads.

-}
maybeToEither :: MonadError e m =>
                 e                      -- ^ (Left e) will be returned if the Maybe value is Nothing
              -> Maybe a                -- ^ (Right a) will be returned if this is (Just a)
              -> m a
maybeToEither errorval Nothing = throwError errorval
maybeToEither _ (Just normalval) = return normalval

{- | Pulls a "Right" value out of an Either value.  If the Either value is
Left, raises an exception with "error". -}
forceEither :: Show e => Either e a -> a
forceEither (Left x) = error (show x)
forceEither (Right x) = x

{- | Like 'forceEither', but can raise a specific message with the error. -}
forceEitherMsg :: Show e => String -> Either e a -> a
forceEitherMsg msg (Left x) = error $ msg ++ ": " ++ show x
forceEitherMsg _ (Right x) = x

{- | Takes an either and transforms it into something of the more generic
MonadError class. -}
eitherToMonadError :: MonadError e m => Either e a -> m a
eitherToMonadError (Left x) = throwError x
eitherToMonadError (Right x) = return x


-- | Take a Left to a value, crashes on a Right
fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft _ = error "Data.Either.Utils.fromLeft: Right"

-- | Take a Right to a value, crashes on a Left
fromRight :: Either a b -> b
fromRight (Right a) = a
fromRight _ = error "Data.Either.Utils.fromRight: Left"

-- | Take an Either, and return the value inside it
fromEither :: Either a a -> a
fromEither (Left a) = a
fromEither (Right a) = a

