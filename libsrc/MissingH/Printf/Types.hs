{- arch-tag: Printf type declarations
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

Portions Copyright (c) 2003 Ian Lynagh and released under the GNU LGPL 2.1.
-}

{- |
   Module     : MissingH.Printf.Types
   Copyright  : Copyright (C) 2004 John Goerzen; (C) 2003 Ian Lynagh
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

This module is used internally by "MissingH.Printf" and is /not intended
to be used in your programs/.

Copyright (c) 2004 John Goerzen, jgoerzen\@complete.org

Portions Copyright (c) 2003 Ian Lynagh
-}

-- Begin John Goerzen's code  
 
module MissingH.Printf.Types where

import System.IO
import Data.Ratio
import Data.FiniteMap

-- data Wrapped a = Wrapped a

{- | All items to be printed must be expressible as one of these. -}
data Value =
           ValueRational Rational
           | ValueString String
           | ValueChar Char
             deriving (Eq, Show, Ord)

showValue :: Value -> String
showValue (ValueRational x) = show x
showValue (ValueChar x) = [x]
showValue (ValueString x) = x

{- | The class to which all items must belong (unless you want to inconvenience
everyone and force them to manually generate 'Value's.
-}
class PFType a where
    toValue :: a -> Value
    fromValue :: Value -> a

instance (Real a) => PFType a where
    toValue = ValueRational . toRational
    fromValue = error "fromValue to generic Real not supported"--fromRational . fromValue

instance PFType Integer where
    toValue = ValueRational . toRational
    fromValue (ValueRational x) = 
        if denominator x == 1
           then toInteger $ numerator x
           else error ("Can't make an int from non-integral rational " ++ show x)
    fromValue _ = error "fromValue integer"

instance PFType Double where
    toValue = ValueRational . toRational
    fromValue (ValueRational x) = fromRational x
    fromValue _ = error "fromValue Double"

instance PFType String where
    toValue = ValueString
    fromValue (ValueString x) = x
    fromValue _ = error "fromValue string"

instance PFType Char where
    toValue = ValueChar
    fromValue (ValueChar x) = x
    fromValue _ = error "fromValue char"

{-
instance PFType Double where
    toValue = ValueDouble
    fromValue (ValueDouble x) = x
    fromValue _ = error "fromValue Double"
-}
{-
instance PFType Value where
    toValue = id
    fromValue = id
-}
class PFRun a where
    pfrun :: ([Value] -> String) -> a
instance PFRun String where
    pfrun f = f $ []
instance (PFType a, PFRun b) => PFRun (a -> b) where
    pfrun f x = 
        let nextfunc xs = f ((toValue x) : xs)
            in
            pfrun nextfunc

class IOPFRun a where
    iopfrun :: Handle -> ([Value] -> String) -> a
instance IOPFRun (IO ()) where
    iopfrun h f = hPutStr h $ pfrun f
instance (PFType a, IOPFRun b) => IOPFRun (a -> b) where
    iopfrun h f x = iopfrun h (\xs -> f (toValue x : xs))

{-
-------------------------------------------
-- Begin code from Ian Lynagh
-- Copyright (c) 2003 Ian Lynagh.  Released under the GNU LGPL 2.1.

Modified November 2004 by John Goerzen:
 * Extraced from Printf sources
 * Removed code I don't need
 * Converted to work without TH
 * Converted to work with MissingH module names
-}

type ConversionFunc = Arg
                   -> [Flag]
                   -> Maybe Width
                   -> Maybe Precision
                   -> String

data Format = Literal String
            | Conversion ConversionFunc
            | CharCount

type ArgNum = Integer
type Arg = Value
type Width = Integer
type Precision = Integer
data Flag = AlternateForm       -- "#"
          | ZeroPadded          -- "0"
          | LeftAdjust          -- "-"
          | BlankPlus           -- " "
          | Plus                -- "+"
          | Thousands           -- "'"
          | AlternativeDigits   -- "I" (ignored)
    deriving (Eq, Show)

xvar :: ArgNum -> String
xvar i = 'x':show i

yvar :: ArgNum -> String
yvar i = 'y':show i

nvar :: ArgNum -> String
nvar i = 'n':show i


