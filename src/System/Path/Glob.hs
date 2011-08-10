{-
Copyright (c) 2006-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : System.Path.Glob
   Copyright  : Copyright (C) 2006-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Functions for expanding wildcards, filenames, and pathnames.

For information on the metacharacters recognized, please see the notes
in "System.Path.WildMatch".

-}

module System.Path.Glob (glob, vGlob)
    where
import Data.List.Utils (hasAny)
import System.IO.HVFS
import System.FilePath (splitFileName)
import Control.Exception (tryJust)
import System.Path.WildMatch (wildCheckCase)
import Data.List (isSuffixOf)

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
expandGlob fs fn =
    case dirnameslash of
      "./" -> runGlob fs "." basename
      "/"  -> do
              rgs <- runGlob fs "/" basename
              return $ map ('/' :) rgs
      _ -> do dirlist <- if hasWild dirname
                             then expandGlob fs dirname
                             else return [dirname]
              if hasWild basename
                 then do r <- mapM expandWildBase dirlist
                         return $ concat r
                 else do r <- mapM expandNormalBase dirlist
                         return $ concat r

    where (dirnameslash, basename) = splitFileName fn
          dirname = case dirnameslash of
                      "/" -> "/"
                      x -> if isSuffixOf "/" x
                              then take (length x - 1) x
                              else x

          expandWildBase :: FilePath -> IO [FilePath]
          expandWildBase dname =
              do dirglobs <- runGlob fs dname basename
                 return $ map withD dirglobs
                 where withD = case dname of
                                 ""  -> id
                                 _   -> \globfn -> dname ++ "/" ++ globfn

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
    where ioErrors :: IOError -> Maybe IOError
          ioErrors e = Just e
