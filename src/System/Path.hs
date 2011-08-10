{-# LANGUAGE CPP #-}
{- arch-tag: Path utilities main file
Copyright (C) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : System.Path
   Copyright  : Copyright (C) 2004-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

This module provides various helpful utilities for dealing with path and
file names, directories, and related support.

Written by John Goerzen, jgoerzen\@complete.org
-}

module System.Path(-- * Name processing
                     splitExt, absNormPath, secureAbsNormPath,
                     -- * Directory Processing
                     recurseDir, recurseDirStat, recursiveRemove,
                     bracketCWD,
                     -- * Temporary Directories
                     mktmpdir, brackettmpdir, brackettmpdirCWD
                    )
where
import Data.List
import Data.List.Utils
#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
import System.Posix.Files
import System.Posix.Directory (createDirectory)
import System.Posix.Temp
import System.Directory hiding (createDirectory)
#else
import System.Directory
#endif
import Control.Exception
import System.IO
import System.Path.NameManip
import System.IO.HVFS.Utils

{- | Splits a pathname into a tuple representing the root of the name and
the extension.  The extension is considered to be all characters from the last
dot after the last slash to the end.  Either returned string may be empty. -}
-- FIXME: See 6.4 API when released.
splitExt :: String -> (String, String)
splitExt path =
    let dotindex = alwaysElemRIndex '.' path
        slashindex = alwaysElemRIndex '/' path
        in
        if dotindex <= slashindex
           then (path, "")
           else ((take dotindex path), (drop dotindex path))

{- | Make an absolute, normalized version of a path with all double slashes,
dot, and dotdot entries removed.

The first parameter is the base for the absolut calculation; in many cases,
it would correspond to the current working directory.

The second parameter is the pathname to transform.  If it is already absolute,
the first parameter is ignored.

Nothing may be returned if there's an error; for instance, too many @..@ entries
for the given path.
-}
absNormPath :: String                   -- ^ Absolute path for use with starting directory
            -> String                   -- ^ The path name to make absolute
            -> Maybe String                   -- ^ Result
absNormPath base thepath =
    let abs = absolute_path_by base thepath
        in case guess_dotdot (normalise_path abs) of
                Just "." -> Just "/"
                x -> x

{- | Like absNormPath, but returns Nothing if the generated result is not
the passed base path or a subdirectory thereof. -}
secureAbsNormPath :: String             -- ^ Absolute path for use with starting directory
                  -> String             -- ^ The path to make absolute
                  -> Maybe String
secureAbsNormPath base s = do p <- absNormPath base s
                              if startswith base p
                                 then return p
                                 else fail ""

{- | Creates a temporary directory for your use.

The passed string should be a template suitable for mkstemp; that is, end with
@\"XXXXXX\"@.

Your string should probably start with the value returned from
System.Directory.getTemporaryDirectory.

The name of the directory created will be returned.
-}
mktmpdir :: String -> IO String
#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
mktmpdir x =
    do y <- mkstemp x
       let (dirname, h) = y
       hClose h
       removeFile dirname
       createDirectory dirname 0o700
       return dirname
#else
#ifdef __GLASGOW_HASKELL__
mktmpdir x =
    do (fp, h) <- openTempFile "" x
       hClose h
       removeFile fp
       createDirectory fp
       return fp
#else
mktmpdir _ = fail "mktmpdir not supported on Windows unless you have GHC"
#endif
#endif

{- | Creates a temporary directory for your use via 'mktmpdir',
runs the specified action (passing in the directory name), then
removes the directory and all its contents when the action completes (or raises
an exception. -}
brackettmpdir :: String -> (String -> IO a) -> IO a
brackettmpdir x action = do tmpdir <- mktmpdir x
                            finally (action tmpdir)
                                    (recursiveRemove SystemFS tmpdir)

{- | Changes the current working directory to the given path,
executes the given I\/O action, then changes back to the original directory,
even if the I\/O action raised an exception. -}
bracketCWD :: FilePath -> IO a -> IO a
bracketCWD fp action =
    do oldcwd <- getCurrentDirectory
       setCurrentDirectory fp
       finally action (setCurrentDirectory oldcwd)

{- | Runs the given I\/O action with the CWD set to the given tmp dir,
removing the tmp dir and changing CWD back afterwards, even if there
was an exception. -}
brackettmpdirCWD :: String -> IO a -> IO a
brackettmpdirCWD template action =
    brackettmpdir template (\newdir -> bracketCWD newdir action)
