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

module MissingH.Printf(-- * Variable-Argument Ouptut
                       vsprintf,
                       vprintf,
                       vfprintf,
                       -- * List-Argument Output
                       sprintf,
                       printf,
                       fprintf,
                       -- * Utility Function
                       v,
                       -- * Underlying Types
                       Value(..),
                       PFRun,
                       PFType(..),
                       IOPFRun,
                       ) where

import MissingH.Str
import Data.List
import System.IO

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

{-
instance PFType Value where
    toValue = id
    fromValue = id
-}

v :: PFType a => a -> Value
v = toValue

class PFRun a where
    pfrun :: ([Value] -> String) -> a
instance PFRun String where
    pfrun f = f $ []
instance (PFType a, PFRun b) => PFRun (a -> b) where
    pfrun f x = pfrun (\xs -> f (toValue x : xs))

class IOPFRun a where
    iopfrun :: Handle -> ([Value] -> String) -> a
instance IOPFRun (IO ()) where
    iopfrun h f = hPutStr h $ pfrun f
instance (PFType a, IOPFRun b) => IOPFRun (a -> b) where
    iopfrun h f x = iopfrun h (\xs -> f (toValue x : xs))

sprintf :: String -> [Value] -> String
sprintf [] [] = []
sprintf ('%' : xs) (y : ys) = (fromValue y) ++ sprintf xs ys
sprintf ('!' : xs) (y : ys) = 
    show (((fromValue y)::Int) + 1) ++ sprintf xs ys
sprintf (x:xs) y = x : sprintf xs y

vsprintf :: (PFRun a) => String -> a
vsprintf f = pfrun $ sprintf f

fprintf :: Handle -> String -> [Value] -> IO ()
fprintf h f v = hPutStr h $ sprintf f v

printf :: String -> [Value] -> IO ()
printf f v = fprintf stdout f v

vfprintf :: IOPFRun a => Handle -> String -> a
vfprintf h f = iopfrun h $ sprintf f

vprintf :: IOPFRun a => String -> a
vprintf f = vfprintf stdout f
