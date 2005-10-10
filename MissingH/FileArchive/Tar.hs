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
   Module     : MissingH.FileArchive.Tar
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Tar file format handler

Copyright (c) 2005 John Goerzen, jgoerzen\@complete.org

-}
module MissingH.FileArchive.Tar (

                                 )
where

import MissingH.Checksum.CRC32.GZip
import MissingH.List
import Data.List
import Data.Bits
import Control.Monad.Error
import Control.Monad.State
import Data.Char
import Data.Word
import MissingH.Bits
import System.IO
import Numeric

type Section = (Header, [Word8])

data GZipError = CRCError               -- ^ CRC-32 check failed
               | NotGZIPFile            -- ^ Couldn't find a GZip header
               | UnknownMethod          -- ^ Compressed with something other than method 8 (deflate)
               | UnknownError String    -- ^ Other problem arose
               deriving (Eq, Show)

instance Error GZipError where
    noMsg = UnknownError ""
    strMsg = UnknownError

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

parseHeader :: String -> (String, String)
parseHeader = 
    runState $
       do name <- (grab 100 >>= rchopstr)
          mode <- (grab 8 >>= rreadoct)
          uid <- (grab 8 >>= rreadoct)
          gid <- (grab 8 >>= rreadoct)
          size <- (grab 8 >>= rreadoct)
          mtime <- (grab 12 >>= rreadoct)
          chksum <- (grab 8 >>= rreadoct)
          typeflag <- grab 1
          linkname <- (grab 100 >>= rchopstr)
          magic <- (grab 6 >>= rchopstr)
          version <- grab 2
          uname <- (grab 32 >>= rchopstr)
          gname <- (grab 32 >>= rchopstr)
          devmajor <- (grab 8 >>= rreadoct)
          devminor <- (grab 8 >>= rreadoct)
          prefix <- (grab 155 >>= rchopstr)
          return "foo"
          
    where chopstr = takeWhile (\c -> c /= '\0')
          rchopstr = return . chopstr
          chopsstr = takeWhile (\c -> c /= ' ') . chopstr
          readoct = fst . head . readOct . chopsstr
          rreadoct = return . readoct