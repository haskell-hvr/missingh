{- arch-tag: Printf utilities main file
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
   Module     : MissingH.Printf
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

This module provides various helpful utilities for using a C-style printf().

Written by John Goerzen, jgoerzen\@complete.org
-}

module MissingH.Printf(
                       ) where

class PFFun a where
              toFun :: a -> PFCall -> String

data Value =
           ValueInt Int
           | ValueString String

data PFCall = PFCall String [Value]

class PFType a where
    toValue :: a -> Value
    fromValue :: Value -> a

instance PFType Int where
    toValue = ValueInt
    fromValue (ValueInt x) = x
    fromValue _ = error "fromValue int"

instance PFType String where
    toValue = ValueString
    fromValue (ValueString x) = x
    fromValue _ = error "fromValue string"


instance PFFun String where
    toFun x (PFCall _ []) = x
    toFun _ _ = error "Too many arguments"

instance (PFType a, PFFun b) => PFFun (a -> b) where
    toFun f (PFCall n (x:xs)) =
        toFun (f (fromValue x)) (PFCall n xs)
    toFun _ _ = error "Too few arguments"

