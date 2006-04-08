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
   Module     : MissingH.Path.Glob
   Copyright  : Copyright (C) 2006 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable

Functions for expanding wildcards, filenames, and pathnames.

For information on the metacharacters recognized, please see the notes
in "MissingH.Path.WildMatch".

-}

module MissingH.Path.Glob(glob, vGlob) where
import MissingH.List
import System.IO
import MissingH.IO.HVFS

hasWild = hasAny "*?["

{- | Takes a pattern.  Returns a list of names that match that pattern.
The pattern is evaluated by "MissingH.Path.WildMatch".  This function
does not perform tilde or environment variable expansion.

Filenames that begin with a dot are not included in the result set unless
that component of the pattern also begins with a dot.

In MissingH, this function is defined as:
>glob = vGlob SystemFS -}
glob :: FilePath -> IO ()
glob = vGlob SystemFS

{- | Like 'glob', but works on both the system ("real") and HVFS virtual
filesystems. -}
vGlob :: HVFS a => a -> FilePath -> IO ()
vGlob fs fn = 
    if not hasWild fn           -- Don't try globbing if there are no wilds
       then do de <- vDoesExist fs fn
               if de
                  then return [fn]
                  else return []
       else expandGlob fs fn -- It's there

expandGlob :: HVFS a => a -> FilePath -> IO ()
exnapdGlob fs fn =
    case dirname of
      "." -> runGlob "." basename
      _ -> do dirlist <- if hasWild dirname
                             then expandGlob fs dirname
                             else return [dirname]
              if hasWild basename
                 then do r <- mapM expandWildBase dirlist
                         return $ concat r
                 else do r <- mapM expandNormalBase dirlist
                         return $ concat r
           
    where (dirname, basename) = splitFileName fn
          expandWildBase :: FilePath -> IO [FilePath]
          expandWildBase dname =
              do dirglobs <- runGlob dname basename
                 return (map \globfn -> dname ++ "/" ++ globfn) dirglobs
          expandNormalBase :: FilePath -> IO [FilePath]
          expandNormalBase dname =
              do isdir <- vDoesDirectoryExist dname
                 if (basename /= "." && basename /= "") || isdir
                    then return [dname ++ "/" ++ basename]
                    else return []

runGlob :: HVFS a => a -> FilePath -> IO ()
runGlob fs "" patt = runGlob fs "." patt
runGlob fs dirname patt =
    case tryJust ioErrors (vGetDirectoryContents fs dirname) of
      Left _ -> []
      Right names -> let matches = return . filter . wildCheckCase $ names
                     in if head patt == '.'
                            then matches
                            else filter (\x -> head x /= '.') matches

                            

    