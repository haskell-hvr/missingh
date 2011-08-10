{-# LANGUAGE CPP #-}
{- arch-tag: HVFS utilities main file
Copyright (c) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : System.IO.HVFS.Utils
   Copyright  : Copyright (C) 2004-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable

This module provides various helpful utilities for dealing 
filesystems.

Written by John Goerzen, jgoerzen\@complete.org

To operate on your system's main filesystem, just pass SystemFS as the
first parameter to these functions.
-}

module System.IO.HVFS.Utils (recurseDir,
                               recurseDirStat,
                               recursiveRemove,
                               lsl,
                               SystemFS(..)
                              )
where

import System.IO.HVFS
import System.Time.Utils
import System.IO.PlafCompat
import Text.Printf
import System.Time
import System.Locale
import System.IO.Unsafe

{- | Obtain a recursive listing of all files\/directories beneath 
the specified directory.  The traversal is depth-first
and the original
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

The items are returned lazily.

WARNING: do not change your current working directory until you have consumed
all the items.  Doing so could cause strange effects.

Alternatively, you may wish to pass an absolute path to this function.
-}

recurseDirStat :: HVFS a => a -> FilePath -> IO [(FilePath, HVFSStatEncap)]
recurseDirStat h fn = 
    do fs <- vGetSymbolicLinkStatus h fn
       if withStat fs vIsDirectory 
          then do
               dirc <- vGetDirectoryContents h fn
               let contents = map ((++) (fn ++ "/")) $ 
                              filter (\x -> x /= "." && x /= "..") dirc
               subdirs <- unsafeInterleaveIO $ mapM (recurseDirStat h) contents
               return $ (concat subdirs) ++ [(fn, fs)]
          else return [(fn, fs)]

{- | Removes a file or a directory.  If a directory, also removes all its
child files\/directories.
-}
recursiveRemove :: HVFS a => a -> FilePath -> IO ()
recursiveRemove h fn =
    recurseDirStat h fn >>= (mapM_ $
        \(fn, fs) -> if withStat fs vIsDirectory 
                         then vRemoveDirectory h fn
                         else vRemoveFile h fn
                              )

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
                          return $ printf "%c%s  1 %-8d %-8d %-9d %s %s%s" 
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
                  
            
