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

module MissingH.Printf(sprintf,
                       Value(..)

                       ) where

import MissingH.Str
import Data.List

class PFFun a where
              toFun :: a -> PFCall -> String

data Value =
           ValueInt Int
           | ValueString String

data PFCall = PFCall [Value]

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
    toFun x (PFCall []) = x
    toFun _ _ = error "Too many arguments"

instance (PFType a, PFFun b) => PFFun (a -> b) where
    toFun f (PFCall (x:xs)) =
        toFun (f (fromValue x)) (PFCall xs)
    toFun _ _ = error "Too few arguments"

class PFRun a where
    pfrun :: ([Value] -> Value) -> a

--instance PFType a => PFRun a where
--    pfrun f = f . fromValue

instance (PFType a, PFRun b) => PFRun (a -> b) where
    pfrun f x = pfrun (\xs -> f (toValue x : xs))

{-
instance PFFun String where
    toFun [] (PFCall []) = []
    toFun ('%' : xs) (PFCall (y:ys)) =
        y : toFun 
    toFun (x:xs) y =
        x : toFun xs y
    -}

{- Sample attempt here -}
sprintf :: String -> [Value] -> String

sprintf [] [] = []
sprintf ('%' : xs) (y : ys) = (fromValue y) ++ sprintf xs ys
sprintf ('!' : xs) (y : ys) = show (((fromValue y)::Int) + 1) ++ sprintf xs ys
sprintf (x:xs) y = x : sprintf xs y

{-
wrapper :: String -> [PFType] -> String
wrapper x v = sprintf x (map toValue v)
-}
{- To try next: define a third pffun instance that itself works off the format string -}
----------------------------------------------------
{-
printf :: String -> PFFun
printf "" = toFun ""
printf ('%' : xs) = toFun (\x -> x ++ printf xs)
printf x = 
    case split "%" x of
         [y] -> toFun y
         [y : z] -> toFun (\a -> y ++ a ++ printf (join "%" z))

               -}