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
                       Value(..),
                       PFRun(..),
                       PFType(..),
                       sprintf,
                       sprintf_real,
--                       ssprintf,
--                       printf,
                       wrapper,
                       v
                       ) where

import MissingH.Str
import Data.List

data Value =
           ValueInt Int
           | ValueString String
             deriving (Eq, Show)

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

instance PFType Value where
    toValue = id
    fromValue = id

v :: PFType a => a -> Value
v = toValue

class PFRun a where
    pfrun :: ([Value] -> String) -> a

instance PFRun String where
    pfrun f = f $ []

instance (PFType a, PFRun b) => PFRun (a -> b) where
    pfrun f x = pfrun (\xs -> f (toValue x : xs))

sprintf_real :: String -> [Value] -> String

sprintf_real [] [] = []
sprintf_real ('%' : xs) (y : ys) = (fromValue y) ++ sprintf_real xs ys
sprintf_real ('!' : xs) (y : ys) = 
    show (((fromValue y)::Int) + 1) ++ sprintf_real xs ys
sprintf_real (x:xs) y = x : sprintf_real xs y

forcestring :: String -> String
forcestring x = x

wrapper :: String -> [Value] -> Value
wrapper f v = toValue $ sprintf_real f v

sprintf :: (PFRun a) => String -> a
sprintf f = pfrun $ sprintf_real f

--printf :: (PFRun a) => String -> IO a
--printf = return . sprintf

--printf :: (PFRun a) => String -> a

--printf :: PFRun a => String -> a -> IO ()

