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
import Data.Bits
import Control.Monad.Error
import Data.Char

type GZipError = String

-- | First two bytes of file
magic = "\x1f\x8b"

-- | Flags
fFTEXT = 1
fFHCRC = 2
fFEXTRA = 4
fFNAME = 8
fFCOMMENT = 16

split1 :: String -> (Char, String)
split1 s = (head s, tail s)

{- | Read the GZip header.  Return (Header, Remainder).
-}
read_header :: String -> Either GZipError (String, String)
read_header s =
    let ok = Right "ok" in
    do let (mag, rem) = splitAt 2 s
       if mag /= magic
          then throwError "Not a GZip file"
          else ok
       let (method, rem) = split1 rem
       if (ord(method) /= 8)
          then throwError "Unknown compression method"
          else ok
       let (flag_S, rem) = split1 rem
       let flag = ord flag_S
       -- skip modtime (4), extraflag (1), and os (1)
       let rem = drop 6 rem
       
       rem <- if (flag .&. fFEXTRA /= 0)
                  then do let (xlen_S, rem2) = split1 rem
                          let (xlen2_S, rem2) = split1 rem2
                          let xlen = (ord xlen_S) + 256 * (ord xlen2_S)
                          return $ drop xlen rem2
                  else return rem
       return ("foo", rem)