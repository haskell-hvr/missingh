{- arch-tag: Maybe utilities
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
   Module     : MissingH.Maybe
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Utilities for working with the Either data type

Copyright (c) 2004 John Goerzen, jgoerzen\@complete.org
-}
module MissingH.Maybe
    (
     forceMaybe
) where

{- | Pulls a Just value out of a Maybe value.  If the Maybe value is
Nothing, raises an exception with error. -}
forceMaybe :: Maybe a -> a
forceMaybe Nothing = error "forceMaybe: Got Nothing"
forceMaybe (Just x) = x
