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
                     splitExt,
                     -- * Directory Processing
                     recurseDir, recurseDirStat
                    )
where
import Data.List
import MissingH.List
import System.Directory
import System.Posix.Files

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

{- | Obtain a recursive listing of all files\/directories beneath 
the specified directory.  The traversal is depth-first and the original
item is always present in the returned list.

If the passed value is not a directory, the return value
be only that value.
-}
recurseDir :: FilePath -> IO [FilePath]
recurseDir x = recurseDirStat x >>= return . map fst

{- | Like 'recurseDir', but return the stat() (System.Posix.Files.FileStatus)
information with them.  This is an optimization if you will be statting files
yourself later.
-}

recurseDirStat :: FilePath -> IO [(FilePath, FileStatus)]
recurseDirStat fn =
    do fs <- getFileStatus fn
       if isDirectory fs then do
                              dirc <- getDirectoryContents fn
                              let contents = map ((++) (fn ++ "/")) $ 
                                     filter (\x -> x /= "." && x /= "..") dirc
                              subdirs <- mapM recurseDirStat contents
                              return $ (concat subdirs) ++ [(fn, fs)]
          else return [(fn, fs)]


                              
                              
          