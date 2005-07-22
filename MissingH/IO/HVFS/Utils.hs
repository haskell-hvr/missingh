{-# LANGUAGE CPP #-}
{- arch-tag: HVFS utilities main file
Copyright (C) 2004-2005 John Goerzen <jgoerzen@complete.org>

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
   Module     : MissingH.IO.HVFS.Utils
   Copyright  : Copyright (C) 2004-2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

This module provides various helpful utilities for dealing 
filesystems.

Written by John Goerzen, jgoerzen\@complete.org

To operate on your system's main filesystem, just pass SystemFS as the
first parameter to these functions.
-}

module MissingH.IO.HVFS.Utils (recurseDir,
                               recurseDirStat,
                               recursiveRemove,
                               lsl,
                               SystemFS(..)
                              )
where

import MissingH.IO.HVFS
import MissingH.Time
import MissingH.IO.PlafCompat
import MissingH.Printf
import System.Time
import System.Locale

{- | Obtain a recursive listing of all files\/directories beneath 
the specified directory.  The traversal is depth-first and the original
item is always present in the returned list.

If the passed value is not a directory, the return value
be only that value.

The \".\" and \"..\" entries are removed from the data returned.
-}
recurseDir :: HVFS a => a -> FilePath -> IO [FilePath]
recurseDir fs x = recurseDirStat fs x >>= return . map fst

{- | Like 'recurseDir', but return the stat() (System.Posix.Files.FileStatus)
information with them.  This is an optimization if you will be statting files
yourself later.
-}

recurseDirStat :: HVFS a => a -> FilePath -> IO [(FilePath, HVFSStatEncap)]
recurseDirStat h fn =
    do fs <- vGetSymbolicLinkStatus h fn
       if withStat fs vIsDirectory then do
                                dirc <- vGetDirectoryContents h fn
                                let contents = map ((++) (fn ++ "/")) $ 
                                     filter (\x -> x /= "." && x /= "..") dirc
                                subdirs <- mapM (recurseDirStat h) contents
                                return $ (concat subdirs) ++ [(fn, fs)]
          else return [(fn, fs)]

{- | Removes a file or a directory.  If a directory, also removes all its
child files\/directories.
-}
recursiveRemove :: HVFS a => a -> FilePath -> IO ()
recursiveRemove h fn =
    let worker [] = return ()
        worker ((fn, fs):xs) =
            do if withStat fs vIsDirectory then
                  vRemoveDirectory h fn
                  else vRemoveFile h fn
               worker xs
        in
        recurseDirStat h fn >>= worker

{- | Provide a result similar to the command ls -l over a directory.

Known bug: setuid bit semantics are inexact compared with standard ls.
-}
lsl :: HVFS a => a -> FilePath -> IO String
lsl fs fp =
    let showmodes mode = 
            let i m = (intersectFileModes mode m /= 0)
                in
                (if i ownerReadMode then 'r' else '-') :
                (if i ownerWriteMode then 'w' else '-') :
                (if i setUserIDMode then 's' else
                    if i ownerExecuteMode then 'x' else '-') :
                (if i groupReadMode then 'r' else '-') :
                (if i groupWriteMode then 'w' else '-') :
                (if i setGroupIDMode then 's' else
                    if i groupExecuteMode then 'x' else '-') :
                (if i otherReadMode then 'r' else '-') :
                (if i otherWriteMode then 'w' else '-') :
                (if i otherExecuteMode then 'x' else '-') : []
        showentry origdir fh (state, fp) = 
            case state of
              HVFSStatEncap se -> 
               let typechar = 
                    if vIsDirectory se then 'd'
                       else if vIsSymbolicLink se then 'l'
                       else if vIsBlockDevice se then 'b'
                       else if vIsCharacterDevice se then 'c'
                       else if vIsSocket se then 's'
                       else if vIsNamedPipe se then 's'
                       else '-'
                   clocktime = epochToClockTime (vModificationTime se)
                   datestr c= formatCalendarTime defaultTimeLocale "%b %e  %Y" 
                               c
                    in do c <- toCalendarTime clocktime
                          linkstr <- case vIsSymbolicLink se of
                                       False -> return ""
                                       True -> do sl <- vReadSymbolicLink fh 
                                                           (origdir ++ "/" ++ fp)
                                                  return $ " -> " ++ sl
                          return $ vsprintf "%c%s  1 %-8d %-8d %-9d %s %s%s" 
                                     typechar
                                     (showmodes (vFileMode se))
                                     (toInteger $ vFileOwner se)
                                     (toInteger $ vFileGroup se)
                                     (toInteger $ vFileSize se)
                                     (datestr c)
                                     fp
                                     linkstr
        in do c <- vGetDirectoryContents fs fp
              pairs <- mapM (\x -> do ss <- vGetSymbolicLinkStatus fs (fp ++ "/" ++ x)
                                      return (ss, x) 
                            ) c
              linedata <- mapM (showentry fp fs) pairs
              return $ unlines $ ["total 1"] ++ linedata
                  
            
