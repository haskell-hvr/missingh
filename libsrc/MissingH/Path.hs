{- arch-tag: Path utilities main file
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
   Module     : MissingH.Path
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

This module provides various helpful utilities for dealing with path and
file names, directories, and related support.

Written by John Goerzen, jgoerzen\@complete.org
-}

module MissingH.Path(-- * Name processing
                     splitExt, absNormPath, secureAbsNormPath,
                     -- * Directory Processing
                     recurseDir, recurseDirStat, recursiveRemove,
                     -- * Temporary Directories
                     mktmpdir, brackettmpdir
                    )
where
import Data.List
import MissingH.List
import System.Directory hiding (createDirectory)
import System.Posix.Files
import System.Posix.Directory (createDirectory)
import System.Posix.Temp
import Control.Exception
import System.IO
import MissingH.Path.NameManip

{- | Splits a pathname into a tuple representing the root of the name and
the extension.  The extension is considered to be all characters from the last
dot after the last slash to the end.  Either returned string may be empty. -}

-- FIXME - See 6.4 API when released.

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

{- | Obtain a recursive listing of all files\/directories beneath 
the specified directory.  The traversal is depth-first and the original
item is always present in the returned list.

If the passed value is not a directory, the return value
be only that value.

The \".\" and \"..\" entries are removed from the data returned.
-}
recurseDir :: FilePath -> IO [FilePath]
recurseDir x = recurseDirStat x >>= return . map fst

{- | Like 'recurseDir', but return the stat() (System.Posix.Files.FileStatus)
information with them.  This is an optimization if you will be statting files
yourself later.
-}

recurseDirStat :: FilePath -> IO [(FilePath, FileStatus)]
recurseDirStat fn =
    do fs <- getSymbolicLinkStatus fn
       if isDirectory fs then do
                              dirc <- getDirectoryContents fn
                              let contents = map ((++) (fn ++ "/")) $ 
                                     filter (\x -> x /= "." && x /= "..") dirc
                              subdirs <- mapM recurseDirStat contents
                              return $ (concat subdirs) ++ [(fn, fs)]
          else return [(fn, fs)]

{- | Removes a file or a directory.  If a directory, also removes all its
child files\/directories.
-}
recursiveRemove :: FilePath -> IO ()
recursiveRemove fn =
    let worker [] = return ()
        worker ((fn, fs):xs) =
            do if isDirectory fs then
                  removeDirectory fn
                  else removeFile fn
               worker xs
        in
        recurseDirStat fn >>= worker

{- | Creates a temporary directory for your use.

The passed string should be a template suitable for mkstemp; that is, end with
@\"XXXXXX\"@.

The name of the directory created will be returned.
-}
mktmpdir :: String -> IO String
mktmpdir x =
    do y <- mkstemp x
       let (dirname, h) = y
       hClose h
       removeFile dirname
       createDirectory dirname 0o700
       return dirname

{- | Creates a temporary directory for your use via 'mktmpdir',
runs the specified action (passing in the directory name), then
removes the directory and all its contents when the action completes (or raises
an exception. -}
brackettmpdir :: String -> (String -> IO a) -> IO a
brackettmpdir x action = do tmpdir <- mktmpdir x
                            finally (action tmpdir) (recursiveRemove tmpdir)
