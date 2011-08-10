{- arch-tag: GZip file support in Haskell
Copyright (c) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : System.FileArchive.GZip
   Copyright  : Copyright (C) 2004-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

GZip file decompression

Copyright (c) 2004 John Goerzen, jgoerzen\@complete.org

The GZip format is described in RFC1952.
-}
module System.FileArchive.GZip (
                                  -- * GZip Files
                                  -- $gzipfiles

                                  -- * Types
                                  Header(..), Section, GZipError(..),
                                  Footer(..),
                                  -- * Whole-File Processing
                                  decompress,
                                  hDecompress,
                                  read_sections,
                                  -- * Section Processing
                                  read_header,
                                  read_section
                                 )
    where

import Data.Compression.Inflate (inflate_string_remainder)
import Data.Hash.CRC32.GZip (update_crc)
import Data.Bits ((.&.))
import Control.Monad.Error -- (Error(), strMsg, throwError)
import Data.Char (ord)
import Data.Word (Word32())
import Data.Bits.Utils (fromBytes)
import System.IO (hGetContents, hPutStr, Handle())

data GZipError = CRCError               -- ^ CRC-32 check failed
               | NotGZIPFile            -- ^ Couldn't find a GZip header
               | UnknownMethod          -- ^ Compressed with something other than method 8 (deflate)
               | UnknownError String    -- ^ Other problem arose
               deriving (Eq, Show)

instance Error GZipError where
    noMsg = UnknownError ""
    strMsg = UnknownError

-- | First two bytes of file
magic :: String
magic = "\x1f\x8b"

-- | Flags
fFHCRC, fFEXTRA, fFNAME, fFCOMMENT :: Int
-- fFTEXT = 1 :: Int
fFHCRC = 2
fFEXTRA = 4
fFNAME = 8
fFCOMMENT = 16

{- | The data structure representing the GZip header.  This occurs
at the beginning of each 'Section' on disk. -}
data Header = Header {
                      method :: Int,    -- ^ Compression method.  Only 8 is defined at present.
                      flags :: Int,
                      extra :: Maybe String,
                      filename :: Maybe String,
                      comment :: Maybe String,
                      mtime :: Word32,  -- ^ Modification time of the original file
                      xfl :: Int,       -- ^ Extra flags
                      os :: Int         -- ^ Creating operating system
                     } deriving (Eq, Show)

{- | Stored on-disk at the end of each section. -}
data Footer = Footer {
                      size :: Word32,   -- ^ The size of the original, decompressed data
                      crc32 :: Word32,  -- ^ The stored GZip CRC-32 of the original, decompressed data
                      crc32valid :: Bool -- ^ Whether or not the stored CRC-32 matches the calculated CRC-32 of the data
                     }

{- | A section represents a compressed component in a GZip file.
Every GZip file has at least one. -}
type Section = (Header, String, Footer)

split1 :: String -> (Char, String)
split1 s = (head s, tail s)

{- | Read a GZip file, decompressing all sections found.

Writes the decompressed data stream to the given output handle.

Returns Nothing if the action was successful, or Just GZipError if there
was a problem.  If there was a problem, the data written to the output
handle should be discarded.
-}

hDecompress :: Handle                   -- ^ Input handle
            -> Handle                   -- ^ Output handle
            -> IO (Maybe GZipError)
hDecompress infd outfd =
    do inc <- hGetContents infd
       let (outstr, err) = decompress inc
       hPutStr outfd outstr
       return err

{- | Read a GZip file, decompressing all sections that are found.

Returns a decompresed data stream and Nothing, or an unreliable string
and Just (error).  If you get anything other than Nothing, the String
returned should be discarded.
-}
decompress :: String -> (String, Maybe GZipError)
{-
decompress s =
    do x <- read_header s
       let rem = snd x
       return $ inflate_string rem
-}
decompress s =
    let procs :: [Section] -> (String, Bool)
        procs [] = ([], True)
        procs ((_, content, foot):xs) =
            let (nexth, nextb) = procs xs in
                (content ++ nexth, (crc32valid foot) && nextb)
        in case read_sections s of
           Left x -> ("", Just x)
           Right x -> let (decomp, iscrcok) = procs x
                          in (decomp, if iscrcok then Nothing else Just CRCError)

{-
decompress s = do x <- read_sections s
                  return $ concatMap (\(_, x, _) -> x) x
-}

-- | Read all sections.
read_sections :: String -> Either GZipError [Section]
read_sections [] = Right []
read_sections s =
    do x <- read_section s
       case x of
           (sect, remain) ->
               do next <- read_sections remain
                  return $ sect : next

parseword :: String -> Word32
parseword s = fromBytes $ map (fromIntegral . ord) $ reverse s

-- | Read one section, returning (ThisSection, Remainder)
read_section :: String -> Either GZipError (Section, String)
read_section s =
        do x <- read_header s
           let headerrem = snd x
           let (decompressed, crc, remainder) = read_data headerrem
           let (crc32str, rm) = splitAt 4 remainder
           let (sizestr, rem2) = splitAt 4 rm
           let filecrc32 = parseword crc32str
           let filesize = parseword sizestr
           return ((fst x, decompressed,
                   Footer {size = filesize, crc32 = filecrc32,
                           crc32valid = filecrc32 == crc})
                   ,rem2)

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
      read_data_internal (y:ys) ck =
        let newcrc = update_crc ck y
            n = newcrc `seq` read_data_internal ys newcrc
            in
            (y : fst n, snd n)



{- | Read the GZip header.  Return (Header, Remainder).
-}
read_header :: String -> Either GZipError (Header, String)
read_header s =
    let ok = Right "ok" in
    do let (mag, rem) = splitAt 2 s
       if mag /= magic
          then throwError NotGZIPFile
          else ok
       let (method, rem2) = split1 rem
       if (ord(method) /= 8)
          then throwError UnknownMethod
          else ok
       let (flag_S, rem3) = split1 rem2
       let flag = ord flag_S
       let (mtimea, rem3a) = splitAt 4 rem3
       let mtime = parseword mtimea
       let (xfla, rem3b) = split1 rem3a
       let xfl = ord xfla
       let (osa, _) = split1 rem3b
       let os = ord osa
       -- skip modtime (4), extraflag (1), and os (1)
       let rem4 = drop 6 rem3

       let (extra, rem5) =
               if (flag .&. fFEXTRA /= 0)
               -- Skip past the extra field if we have it.
                  then let (xlen_S, _) = split1 rem4
                           (xlen2_S, rem4b) = split1 rem4
                           xlen = (ord xlen_S) + 256 * (ord xlen2_S)
                           (ex, rrem) = splitAt xlen rem4b
                           in (Just ex, rrem)
                  else (Nothing, rem4)

       let (filename, rem6) =
               if (flag .&. fFNAME /= 0)
               -- Skip past the null-terminated filename
                  then let fn = takeWhile (/= '\x00') rem5
                                in (Just fn, drop ((length fn) + 1) rem5)
                  else (Nothing, rem5)

       let (comment, rem7) =
               if (flag .&. fFCOMMENT /= 0)
                  -- Skip past the null-terminated comment
                  then let cm = takeWhile (/= '\x00') rem6
                           in (Just cm, drop ((length cm) + 1) rem6)
                  else (Nothing, rem6)

       rem8 <- if (flag .&. fFHCRC /= 0)
                  -- Skip past the header CRC
                  then return $ drop 2 rem7
                  else return rem7

       return (Header {method = ord method,
                      flags = flag,
                      extra = extra,
                      filename = filename,
                      comment = comment,
                      mtime = mtime,
                      xfl = xfl,
                      os = os}, rem8)

----------------------------------------------------------------------
-- Documentation
----------------------------------------------------------------------

{- $gzipfiles

GZip files contain one or more 'Section's.  Each 'Section', on disk, begins
with a GZip 'Header', then stores the compressed data itself, and finally
stores a GZip 'Footer'.

The 'Header' identifies the file as a GZip file, records the original
modification date and time, and, in some cases, also records the original
filename and comments.

The 'Footer' contains a GZip CRC32 checksum over the decompressed data as
well as a 32-bit length of the decompressed data.  The module
'Data.Hash.CRC32.GZip' is used to validate stored CRC32 values.

The vast majority of GZip files contain only one 'Section'.  Standard tools
that work with GZip files create single-section files by default.

Multi-section files can be created by simply concatenating two existing
GZip files together.  The standard gunzip and zcat tools will simply
concatenate the decompressed data when reading these files back.  The
'decompress' function in this module will do the same.

When reading data from this module, please use caution regarding how you access
it.  For instance, if you are wanting to write the decompressed stream
to disk and validate its CRC32 value, you could use the 'decompress'
function.  However, you should process the entire stream before you check
the value of the Bool it returns.  Otherwise, you will force Haskell to buffer
the entire file in memory just so it can check the CRC32.
-}
