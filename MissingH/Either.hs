{- arch-tag: Euither utilities
Copyright (C) 2004 John Goerzen <jgoerzen@complete.org>

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
   Module     : MissingH.Either
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Utilities for working with the Either data type

Copyright (c) 2004 John Goerzen, jgoerzen\@complete.org
-}
module MissingH.Either
    (
     maybeToEither,
     forceEither,
     eitherToMonadError
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

{- | Takes an either and transforms it into something of the more generic
MonadError class. -}
eitherToMonadError :: MonadError e m => Either e a -> m a
eitherToMonadError (Left x) = throwError x
eitherToMonadError (Right x) = return x
