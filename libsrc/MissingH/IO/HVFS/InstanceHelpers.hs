{- arch-tag: HVFS instance helpers
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
   Module     : MissingH.IO.HVFS.InstanceHelpers
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Utilities for creating instances of the items defined in
"MissingH.IO.HVFS".

Copyright (c) 2004 John Goerzen, jgoerzen\@complete.org
-}

module MissingH.IO.HVFS.InstanceHelpers(-- * HVFSStat objects
                                        SimpleStat(..),
                                        -- * HVFS objects & types
                                        MemoryVFS,
                                        newMemoryVFS,
                                        MemoryNode,
                                        MemoryEntry(..),
                                       )
where
import MissingH.IO.HVFS
import Data.IORef
import MissingH.Path
import MissingH.Path.NameManip
import Control.Monad.Error
import System.IO.Error

{- | A simple class that assumes that everything is either a file
or a directory. -}
data SimpleStat = SimpleStat {
                              isFile :: Bool -- ^ True if file, False if directory
                             } deriving (Show, Eq)
instance HVFSStat SimpleStat where
    vIsRegularFile x = isFile x
    vIsDirectory x = not (isFile x)

----------------------------------------------------------------------
-- In-Memory Tree Types
----------------------------------------------------------------------

type MemoryNode = (String, MemoryEntry)
data MemoryEntry = MemoryDirectory [MemoryNode]
                 | MemoryFile String
data MemoryVFS = MemoryVFS 
               { content :: IORef [MemoryNode],
                 cwd :: IORef FilePath
               }

-- | Create a new 'MemoryVFS' object from an existing tree.
-- An empty filesystem may be created by using @[]@ for the parameter.
newMemoryVFS :: [MemoryNode] -> IO MemoryVFS
newMemoryVFS s = do r <- newIORef s
                    newMemoryVFSRef r

-- | Create a new 'MemoryVFS' object using an IORef to an
-- existing tree.
newMemoryVFSRef :: IORef [MemoryNode] -> IO MemoryVFS
newMemoryVFSRef r = do
                    c <- newIORef "/"
                    return (MemoryVFS {content = r, cwd = c})

-- | Find an element on the tree, assuming a normalized path
findMelem :: MemoryVFS -> String -> IO MemoryEntry
findMelem x "/" = readIORef (content x) >>= return . MemoryDirectory
findMelem x path =
    let sliced1 = slice_path path
        h = head sliced1
        t = tail sliced1
        newh = if (h /= "/") && head h == '/' then tail h else h
        sliced2 = newh : t
                  
        -- Walk the tree
        walk :: MemoryEntry -> [String] -> Either String MemoryEntry
        -- Empty list -- return the item we have
        walk y [] = Right y
        -- Root directory -- return the item we have
        walk y ["/"] = Right y
        -- File but stuff: error
        walk (MemoryFile _) (x : _) = 
            Left $ "Attempt to look up name " ++ x ++ " in file"
        walk (MemoryDirectory y) (x : xs) =
            let newentry = case lookup x y of
                                Nothing -> Left $ "Couldn't find entry " ++ x
                                Just z -> Right z
                in do newobj <- newentry
                      walk newobj xs
        in do
           c <- readIORef $ content x
           case walk (MemoryDirectory c) (slice_path path) of
              Left err -> fail err
              Right result -> return result

-- | Find an element on the tree, normalizing the path first
getMelem :: MemoryVFS -> String -> IO MemoryEntry
getMelem x s = 
    do base <- readIORef $ cwd x
       case absNormPath base s of
           Nothing -> fail $ "Trouble normalizing path " ++ s
           Just newpath -> findMelem x newpath

instance HVFS MemoryVFS where
    vGetCurrentDirectory x = readIORef $ cwd x
    vSetCurrentDirectory x fp =
        do curpath <- vGetCurrentDirectory x
           -- Make sure new dir is valid
           newdir <- getMelem x fp
           case newdir of 
               (MemoryFile _) -> fail $ "Attempt to cwd to non-directory " ++ fp
               (MemoryDirectory _) -> 
                   case absNormPath curpath fp of
                       Nothing -> fail $ "Bad internal error"
                       Just y -> writeIORef (cwd x) y
    vGetFileStatus x fp = 
        do elem <- getMelem x fp
           case elem of
                     (MemoryFile _) -> return $ HVFSStatEncap $
                                             SimpleStat {isFile = True}
                     (MemoryDirectory _) -> return $ HVFSStatEncap $
                                             SimpleStat {isFile = False}
    vGetDirectoryContents x fp =
        do elem <- getMelem x fp
           case elem of
                MemoryFile _ -> vRaiseError x doesNotExistErrorType
                                  "Can't list contents of a file"
                                  (Just fp)
                MemoryDirectory c -> return $ map fst c
