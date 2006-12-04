{- Copyright (C) 2006 John Goerzen <jgoerzen@complete.org>

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
   Module     : System.Path.Glob
   Copyright  : Copyright (C) 2006 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable

Functions for expanding wildcards, filenames, and pathnames.

For information on the metacharacters recognized, please see the notes
in "System.Path.WildMatch".

-}

module System.Path.Glob(glob, vGlob) where
import Data.List.Utils
import System.IO
import System.IO.HVFS
import System.FilePath.Version_0_11
import Control.Exception
import System.Path.WildMatch

hasWild = hasAny "*?["

{- | Takes a pattern.  Returns a list of names that match that pattern.
The pattern is evaluated by "System.Path.WildMatch".  This function
does not perform tilde or environment variable expansion.

Filenames that begin with a dot are not included in the result set unless
that component of the pattern also begins with a dot.

In MissingH, this function is defined as:

>glob = vGlob SystemFS
-}
glob :: FilePath -> IO [FilePath]
glob = vGlob SystemFS

{- | Like 'glob', but works on both the system ("real") and HVFS virtual
filesystems. -}
vGlob :: HVFS a => a -> FilePath -> IO [FilePath]
vGlob fs fn = 
    if not (hasWild fn)           -- Don't try globbing if there are no wilds
       then do de <- vDoesExist fs fn
               if de
                  then return [fn]
                  else return []
       else expandGlob fs fn -- It's there

expandGlob :: HVFS a => a -> FilePath -> IO [FilePath]
expandGlob fs fn =
    case dirname of
      "." -> runGlob fs "." basename
      _ -> do dirlist <- if hasWild dirname
                             then expandGlob fs dirname
                             else return [dirname]
              if hasWild basename
                 then do r <- mapM expandWildBase dirlist
                         return $ concat r
                 else do r <- mapM expandNormalBase dirlist
                         return $ concat r
           
    where basename = takeBaseName fn
          dirname = takeDirectory fn
          expandWildBase :: FilePath -> IO [FilePath]
          expandWildBase dname =
              do dirglobs <- runGlob fs dname basename
                 return $ map (\globfn -> dname ++ "/" ++ globfn) dirglobs
          expandNormalBase :: FilePath -> IO [FilePath]
          expandNormalBase dname =
              do isdir <- vDoesDirectoryExist fs dname
                 let newname = dname ++ "/" ++ basename
                 isexists <- vDoesExist fs newname
                 if isexists && ((basename /= "." && basename /= "") || isdir)
                    then return [dname ++ "/" ++ basename]
                    else return []

runGlob :: HVFS a => a -> FilePath -> FilePath -> IO [FilePath]
runGlob fs "" patt = runGlob fs "." patt
runGlob fs dirname patt =
    do r <- tryJust ioErrors (vGetDirectoryContents fs dirname)
       case r of 
         Left _ -> return []
         Right names -> let matches = filter (wildCheckCase patt) $ names
                        in if head patt == '.'
                           then return matches
                           else return $ filter (\x -> head x /= '.') matches
