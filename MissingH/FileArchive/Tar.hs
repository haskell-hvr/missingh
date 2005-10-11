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

