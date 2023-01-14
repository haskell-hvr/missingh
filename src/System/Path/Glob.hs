{-
Copyright (c) 2006-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : System.Path.Glob
   Copyright  : Copyright (C) 2006-2011 John Goerzen
   SPDX-License-Identifier: BSD-3-Clause

   Stability  : stable
   Portability: portable

Functions for expanding wildcards, filenames, and pathnames.

For information on the metacharacters recognized, please see the notes
in "System.Path.WildMatch".

-}

module System.Path.Glob (glob, vGlob)
    where

import           Control.Exception     (tryJust)
import           Data.List             (isSuffixOf)
import           Data.List.Utils       (hasAny)
import           System.FilePath       (pathSeparator, splitFileName, (</>))
import           System.IO.HVFS        (HVFS (vDoesDirectoryExist, vDoesExist, vGetDirectoryContents),
                                         SystemFS (SystemFS))
import           System.Path.WildMatch (wildCheckCase)

hasWild :: String -> Bool
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
expandGlob fs fn
    | dirnameslash == '.':pathSeparator:[] = runGlob fs "." basename
    | dirnameslash == [pathSeparator] = do
                        rgs <- runGlob fs [pathSeparator] basename
                        return $ map (pathSeparator :) rgs
    | otherwise = do dirlist <- if hasWild dirname
                                  then expandGlob fs dirname
                                  else return [dirname]
                     if hasWild basename
                       then concat `fmap` mapM expandWildBase dirlist
                       else concat `fmap` mapM expandNormalBase dirlist

    where (dirnameslash, basename) = splitFileName fn
          dirname = if dirnameslash == [pathSeparator]
                      then [pathSeparator]
                      else if isSuffixOf [pathSeparator] dirnameslash
                              then init dirnameslash
                              else dirnameslash

          expandWildBase :: FilePath -> IO [FilePath]
          expandWildBase dname =
              do dirglobs <- runGlob fs dname basename
                 return $ map withD dirglobs
                 where withD = case dname of
                                 ""  -> id
                                 _   -> \globfn -> dname ++ [pathSeparator] ++ globfn

          expandNormalBase :: FilePath -> IO [FilePath]
          expandNormalBase dname =
              do isdir <- vDoesDirectoryExist fs dname
                 let newname = dname </> basename
                 isexists <- vDoesExist fs newname
                 if isexists && ((basename /= "." && basename /= "") || isdir)
                    then return [dname </> basename]
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
    where ioErrors :: IOError -> Maybe IOError
          ioErrors e = Just e
