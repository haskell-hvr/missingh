{- arch-tag: Varargs main file
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
   Module     : MissingH.Varargs
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable


Copyright (c) 2004 John Goerzen, jgoerzen\@complete.org

Inspiration and ideas from haskell-xml-rpc by Bjorn Bringert

-}

module MissingH.Varargs where

import Data.Dynamic
import Data.Typeable

isfunc :: Typeable a => a -> Bool
isfunc x = (length $ typerepArgs $ typeOf x) > 1
    

--callit :: (a -> b) -> ([b] -> c) -> c
--callit :: Typeable (a, b, c, d) => (a -> b) -> c -> d

data EndOrRecurse a b = Recurse (a -> EndOrRecurse a b)
                      | End b
                        deriving (Eq, Typeable)

printeor :: EndOrRecurse a String -> String
printeor (End x) = "End " ++ x
printeor (Recurse _) = "Recurse"

--toeor :: (Typeable a, Typeable b, Typeable c, Typeable (EndOrRecurse b c)) => a -> EndOrRecurse b c
toeor f =
    let d = toDyn f 
        in
        if isfunc f then
           Recurse (case fromDynamic d of
                    Nothing -> error "Couldn't get func back for recurse"
                    Just x -> x)
        else End (case fromDynamic d of
                  Nothing -> error "Couldn't get item back for end"
                  Just x -> x)

test = (toeor "foo")::EndOrRecurse String String
