{-# LANGUAGE CPP #-}
{- arch-tag: HVFS Combinators
Copyright (c) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}
{- |
   Module     : System.IO.HVFS.Combinators
   Copyright  : Copyright (C) 2004-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Support for combining different HVFS modules together

Copyright (c) 2004-2005 John Goerzen, jgoerzen\@complete.org

-}

module System.IO.HVFS.Combinators ( -- * Restrictions
                                    HVFSReadOnly(..),
                                    HVFSChroot, newHVFSChroot)
    where

import System.IO
import System.IO.Error
import System.IO.HVFS
import System.IO.HVFS.InstanceHelpers (getFullPath)
#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
import System.Posix.Files -- This actually needed? -Wall doesn't seem to think
                          -- so, but I'm not sure...
#endif
import System.Path (secureAbsNormPath)
import System.Path.NameManip (normalise_path)

----------------------------------------------------------------------
-- Providing read-only access
----------------------------------------------------------------------
{- | Restrict access to the underlying filesystem to be strictly
read-only.  Any write-type operations will cause an error.

No constructor is required; just say @HVFSReadOnly fs@ to make a
new read-only wrapper around the 'HVFS' instance @fs@.
-}
data HVFS a => HVFSReadOnly a = HVFSReadOnly a
                              deriving (Eq, Show)
withro :: HVFS a => (a -> b) -> HVFSReadOnly a -> b
withro f (HVFSReadOnly x) = f x

roerror :: (HVFS a) => HVFSReadOnly a -> IO c
roerror h =
    let err x = vRaiseError x permissionErrorType "Read-only virtual filesystem"
                  Nothing
        in withro err h

instance HVFS a => HVFS (HVFSReadOnly a) where
    vGetCurrentDirectory = withro vGetCurrentDirectory
    vSetCurrentDirectory = withro vSetCurrentDirectory
    vGetDirectoryContents = withro vGetDirectoryContents
    vDoesFileExist = withro vDoesFileExist
    vDoesDirectoryExist = withro vDoesDirectoryExist
    vCreateDirectory h _ = roerror h
    vRemoveDirectory h _ = roerror h
    vRenameDirectory h _ _ = roerror h
    vRenameFile h _ _ = roerror h
    vGetFileStatus = withro vGetFileStatus
    vGetSymbolicLinkStatus = withro vGetSymbolicLinkStatus
    vGetModificationTime = withro vGetModificationTime
    vRaiseError = withro vRaiseError
    vCreateSymbolicLink h _ _ = roerror h
    vReadSymbolicLink = withro vReadSymbolicLink
    vCreateLink h _ _ = roerror h

instance HVFSOpenable a => HVFSOpenable (HVFSReadOnly a) where
    vOpen fh fp mode =
        case mode of ReadMode -> withro (\h -> vOpen h fp mode) fh
                     _ -> roerror fh

----------------------------------------------------------------------
-- Restricting to a subdirectory
----------------------------------------------------------------------
{- | Access a subdirectory of a real filesystem as if it was the root
of that filesystem. -}
data HVFS a => HVFSChroot a = HVFSChroot String a
                            deriving (Eq, Show)

{- | Create a new 'HVFSChroot' object. -}
newHVFSChroot :: HVFS a => a            -- ^ The object to pass requests on to
              -> FilePath               -- ^ The path of the directory to make root
              -> IO (HVFSChroot a)      -- ^ The resulting new object
newHVFSChroot fh fp =
    do full <- getFullPath fh fp
       isdir <- vDoesDirectoryExist fh full
       if isdir
          then do let newobj = (HVFSChroot full fh)
                  vSetCurrentDirectory newobj "/"
                  return newobj
          else vRaiseError fh doesNotExistErrorType
                 ("Attempt to instantiate HVFSChroot over non-directory " ++ full)
                 (Just full)

{- | Get the embedded object -}
dch :: (HVFS t) => HVFSChroot t -> t
dch (HVFSChroot _ a) = a

{- | Convert a local (chroot) path to a full path. -}
dch2fp, fp2dch :: (HVFS t) => HVFSChroot t -> String -> IO String
dch2fp mainh@(HVFSChroot fp h) locfp =
    do full <- case (head locfp) of
                  '/' -> return (fp ++ locfp)
                  _ -> do y <- getFullPath mainh locfp
                          return $ fp ++ y
       case secureAbsNormPath fp full of
           Nothing -> vRaiseError h doesNotExistErrorType
                        ("Trouble normalizing path in chroot")
                        (Just (fp ++ "," ++ full))
           Just x -> return x

{- | Convert a full path to a local (chroot) path. -}
fp2dch (HVFSChroot fp h) locfp =
    do newpath <- case secureAbsNormPath fp locfp of
                     Nothing -> vRaiseError h doesNotExistErrorType
                                  ("Unable to securely normalize path")
                                  (Just (fp ++ "/" ++ locfp))
                     Just x -> return x
       if (take (length fp) newpath /= fp)
               then vRaiseError h doesNotExistErrorType
                        ("Local path is not subdirectory of parent path")
                        (Just newpath)
               else let newpath2 = drop (length fp) newpath
                        in return $ normalise_path ("/" ++ newpath2)

dch2fph :: (HVFS t) => (t -> String -> IO t1) -> HVFSChroot t -> [Char] -> IO t1
dch2fph func fh@(HVFSChroot _ h) locfp =
    do newfp <- dch2fp fh locfp
       func h newfp

instance HVFS a => HVFS (HVFSChroot a) where
    vGetCurrentDirectory x = do fp <- vGetCurrentDirectory (dch x)
                                fp2dch x fp
    vSetCurrentDirectory = dch2fph vSetCurrentDirectory
    vGetDirectoryContents = dch2fph vGetDirectoryContents
    vDoesFileExist = dch2fph vDoesFileExist
    vDoesDirectoryExist = dch2fph vDoesDirectoryExist
    vCreateDirectory = dch2fph vCreateDirectory
    vRemoveDirectory = dch2fph vRemoveDirectory
    vRenameDirectory fh old new = do old' <- dch2fp fh old
                                     new' <- dch2fp fh new
                                     vRenameDirectory (dch fh) old' new'
    vRemoveFile = dch2fph vRemoveFile
    vRenameFile fh old new = do old' <- dch2fp fh old
                                new' <- dch2fp fh new
                                vRenameFile (dch fh) old' new'
    vGetFileStatus = dch2fph vGetFileStatus
    vGetSymbolicLinkStatus = dch2fph vGetSymbolicLinkStatus
    vGetModificationTime = dch2fph vGetModificationTime
    -- vRaiseError
    vCreateSymbolicLink fh old new = do old' <- dch2fp fh old
                                        new' <- dch2fp fh new
                                        vCreateSymbolicLink (dch fh) old' new'
    vReadSymbolicLink fh fp = do result <- dch2fph vReadSymbolicLink fh fp
                                 fp2dch fh result
    vCreateLink fh old new = do old' <- dch2fp fh old
                                new' <- dch2fp fh new
                                vCreateLink (dch fh) old' new'

instance HVFSOpenable a => HVFSOpenable (HVFSChroot a) where
    vOpen fh fp mode = do newfile <- dch2fp fh fp
                          vOpen (dch fh) newfile mode
