-- arch-tag: GZip file support in Haskell

{- |
   Module     : MissingH.FileArchive.GZip
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen,
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

GZip file decompression

Copyright (C) 2004 John Goerzen, jgoerzen\@complete.org

The GZip format is described in RFC1952

-}

module MissingH.FileArchive.GZip (
                                 )
where

import MissingH.Compression.Inflate
import MissingH.Checksum.CRC32
import Data.List
import Control.Monad.Error

type GZipError = String

-- | First two bytes of file
magic = "\x1f\x8b"

{- | Read the GZip header.  Return (Header, Remainder).
-}
read_header :: String -> Either GZipError (String, String)
read_header s =
    let ok = Right "ok" in
    do let (mag, rem) = splitAt 2 s
       if mag /= magic
          then throwError "Not a GZip file"
          else ok
       return ("test", rem)