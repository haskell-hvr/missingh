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
                                  decompress,
                                  read_header
                                 )
where

import MissingH.Compression.Inflate
import MissingH.Checksum.CRC32.GZip
import Data.List
import Data.Bits
import Control.Monad.Error
import Data.Char
import Data.Word
import MissingH.Bits

type GZipError = String

-- | First two bytes of file
magic = "\x1f\x8b"

-- | Flags
fFTEXT = 1::Int
fFHCRC = 2::Int
fFEXTRA = 4::Int
fFNAME = 8::Int
fFCOMMENT = 16::Int

data Header = Header {
                      method :: Int,
                      flags :: Int,
                      extra :: Maybe String,
                      filename :: Maybe String,
                      comment :: Maybe String
                     }

split1 :: String -> (Char, String)
split1 s = (head s, tail s)

{- | Read a GZip file.
-}

decompress :: String -> Either GZipError String
{-
decompress s = 
    do x <- read_header s
       let rem = snd x
       return $ inflate_string rem
-}

decompress s = do x <- read_sections s
                  return $ concatMap snd x


-- | Read all sections.  Returns (Header, ThisSection)
read_sections :: String -> Either GZipError [(Header, String)]
read_sections [] = Right []
read_sections s = do x <- read_section s
                     case x of
                            (head, this, remain) -> do 
                                                    next <- read_sections remain
                                                    return $ (head, this) : next

-- | Read one section, returning (Header, ThisSection, Remainder)
read_section :: String -> Either GZipError (Header, String, String)
read_section s =
        do x <- read_header s
           let headerrem = snd x
           let (decompressed, crc32, remainder) = read_data headerrem
           let (crc32str, rem) = splitAt 4 remainder
           let (sizestr, rem2) = splitAt 4 rem
           let filecrc32 = fromBytes $
                           map (fromIntegral . ord) $ reverse crc32str
           
           if filecrc32 == crc32 
              then return $ (fst x, decompressed, rem2)
              else throwError $ "CRC MISMATCH; calculated: " ++
                                (show crc32)
                                ++ ", recorded: " ++ (show filecrc32)
           
    

-- | Read the file's compressed data, returning
-- (Decompressed, Calculated CRC32, Remainder)
read_data :: String -> (String, Word32, String)
read_data x = 
    let (decompressed1, remainder) = inflate_string_remainder x
        (decompressed, crc32) = read_data_internal decompressed1 0
        in
        (decompressed, crc32, remainder)
    where
    read_data_internal [] ck = ([], ck)
    read_data_internal (x:xs) ck =
        let n = read_data_internal xs (update_crc ck x)
            in
            (x : fst n, snd n)
    


{- | Read the GZip header.  Return (Header, Remainder).
-}
read_header :: String -> Either GZipError (Header, String)
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
                  
       return (Header {method = ord method,
                      flags = flag,
                      extra = Nothing,
                      filename = Nothing,
                      comment = Nothing}, rem8)
