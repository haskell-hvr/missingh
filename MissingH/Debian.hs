{- arch-tag: Debian Package utilities main file
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
   Module     : MissingH.Debian
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

This module provides various helpful utilities for dealing with Debian
files and programs.

Written by John Goerzen, jgoerzen\@complete.org
-}

module MissingH.Debian (-- * Control or Similar File Utilities
                        ControlFile,
                        -- * Version Number Utilities
                        DebVersion, compareDebVersion, checkDebVersion
                       )
where
import System.Cmd
import MissingH.Debian.ControlParser
import MissingH.Cmd
import MissingH.Str
import System.IO.Unsafe
import System.Exit

{- | The type representing the contents of a Debian control file,
or any control-like file (such as the output from apt-cache show, etc.) -}
type ControlFile = [(String, String)]

splitComma :: String -> [String]
splitComma = map strip . split ","

----------------------------------------------------------------------
-- VERSION NUMBERS
----------------------------------------------------------------------

{- | The type representing a Debian version number.  This type is an instance
of 'Prelude.Ord', but you can also use 'compareDebVersion' if you prefer. -}
data DebVersion = DebVersion String
                deriving (Eq)
instance Ord DebVersion where
    compare (DebVersion v1) (DebVersion v2) = 
        {- This is OK since compareDebVersion should always be the same. -}
        unsafePerformIO $ compareDebVersion v1 v2

{- | Compare the versions of two packages. -}
compareDebVersion :: String -> String -> IO Ordering
compareDebVersion v1 v2 =
    let runit op = checkDebVersion v1 op v2
        in do islt <- runit "lt"
              if islt
                 then return LT
                 else do isgt <- runit "gt"
                         if isgt
                            then return GT
                            else return EQ

checkDebVersion :: String       -- ^ Version 1
                -> String       -- ^ Operator
                -> String       -- ^ Version 2
                -> IO Bool
checkDebVersion v1 op v2 =
    do ec <- rawSystem "dpkg" ["--compare-versions", v1, op, v2]
       case ec of 
               ExitSuccess -> return True
               ExitFailure _ -> return False
