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
                                  decompress
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
fFTEXT = 1::Int
fFHCRC = 2::Int
fFEXTRA = 4::Int
fFNAME = 8::Int
fFCOMMENT = 16::Int

split1 :: String -> (Char, String)
split1 s = (head s, tail s)

{- | Read a GZip file.
-}

decompress :: String -> Either GZipError String
decompress s = 
    do x <- read_header s
       let rem = snd x
       return $ inflate_string rem

{- | Read the GZip header.  Return (Header, Remainder).
-}
read_header :: String -> Either GZipError (String, String)
read_header s =
    let ok = Right "ok" in
    do let (mag, rem) = splitAt 2 s
       if mag /= magic
          then throwError "Not a GZip file"
          else ok
       let (method, rem2) = split1 rem
       if (ord(method) /= 8)
          then throwError "Unknown compression method"
          else ok
       let (flag_S, rem3) = split1 rem2
       let flag = ord flag_S
       -- skip modtime (4), extraflag (1), and os (1)
       let rem4 = drop 6 rem3
       
       rem5 <- if (flag .&. fFEXTRA /= 0)
                  -- Skip past the extra field if we have it.
                  then do let (xlen_S, rem4a) = split1 rem4
                          let (xlen2_S, rem4a) = split1 rem4
                          let xlen = (ord xlen_S) + 256 * (ord xlen2_S)
                          return $ drop xlen rem4a
                  else return rem4
       
       rem6 <- if (flag .&. fFNAME /= 0)
                  -- Skip past the null-terminated filename
                  then return $ tail $ dropWhile (/= '\x00') rem5
                  else return rem5

       rem7 <- if (flag .&. fFCOMMENT /= 0)
                  -- Skip past the null-terminated comment
                  then return $ tail $ dropWhile (/= '\x00') rem6
                  else return rem6
       
       rem8 <- if (flag .&. fFHCRC /= 0)
                  -- Skip past the header CRC
                  then return $ drop 2 rem7
                  else return rem7
                  
       return ("foo", rem8)
