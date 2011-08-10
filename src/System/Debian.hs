{- arch-tag: Debian Package utilities main file
Copyright (c) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : System.Debian
   Copyright  : Copyright (C) 2004-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

This module provides various helpful utilities for dealing with Debian
files and programs.

Written by John Goerzen, jgoerzen\@complete.org
-}

module System.Debian (-- * Control or Similar File Utilities
                        ControlFile,
                        -- * Version Number Utilities
                        DebVersion, compareDebVersion, checkDebVersion
                       )
    where

import System.Cmd
import System.IO.Unsafe
import System.Exit

{- | The type representing the contents of a Debian control file,
or any control-like file (such as the output from apt-cache show, etc.) -}
type ControlFile = [(String, String)]

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
