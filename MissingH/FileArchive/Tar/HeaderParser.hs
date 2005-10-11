{-
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

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
   Module     : MissingH.FileArchive.Tar.HeaderParser
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Parser for tar-format headers.

Copyright (c) 2005 John Goerzen, jgoerzen\@complete.org

-}
module MissingH.FileArchive.Tar.HeaderParser
    (

     )

where

import Text.ParserCombinators.Parsec
import Data.Word
import Numeric

{- | The data structure representing the Tar header.  This occurs
at the beginning of each 'Section'. -}
data Header = 
    UStar {
           name :: String,
           mode :: Int,
           uid :: Int,
           gid :: Int,
           size :: Integer,
           mtime :: Integer,
           chksum :: Word32,
           typeflag :: Char,
           linkname :: String,
           magic :: String,
           version :: String,
           uname :: String,
           gname :: String,
           devmajor :: Integer,
           devminor :: Integer,
           prefix :: String}
    deriving (Eq, Show)

parseUStarHeader :: CharParser st Header
parseUStarHeader =
    do name <- (grab 100 >>= rchopstr)
       mode <- (grab 8 >>= rreadoct)
       uid <- (grab 8 >>= rreadoct)
       gid <- (grab 8 >>= rreadoct)
       size <- (grab 8 >>= rreadoct)
       mtime <- (grab 12 >>= rreadoct)
       chksum <- (grab 8 >>= rreadoct)
       typeflag <- anyChar
       linkname <- (grab 100 >>= rchopstr)
       string "ustar\0"         -- Magic
       string "00"              -- Version
       uname <- (grab 32 >>= rchopstr)
       gname <- (grab 32 >>= rchopstr)
       devmajor <- (grab 8 >>= rreadoct)
       devminor <- (grab 8 >>= rreadoct)
       prefix <- (grab 155 >>= rchopstr)

       return $ UStar  {name = name, mode = mode, uid = uid, gid = gid,
                        size = size, mtime = mtime, chksum = chksum,
                        typeflag = typeflag, linkname = linkname,
                        magic = "", version = "",
                        uname = uname, gname = gname, devmajor = devmajor,
                        devminor = devminor, prefix = prefix}
    where grab n = count n anyChar
          chopstr = takeWhile (\c -> c /= '\0')
          rchopstr = return . chopstr
          chopsstr = takeWhile (\c -> c /= ' ') . chopstr
          readoct :: (Num a) => String -> a
          readoct = fst . head . readOct . chopsstr
          rreadoct :: (Num a, Monad m) => String -> m a
          rreadoct = return . readoct