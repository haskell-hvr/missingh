{- arch-tag: I/O utilities, binary tools
Copyright (c) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : System.IO.Binary
   Copyright  : Copyright (C) 2004-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable to platforms supporting binary I\/O

This module provides various helpful utilities for dealing with binary
input and output.

You can use this module to deal with binary blocks of data as either Strings
or lists of Word8.  The BinaryConvertible class provides this abstraction.

Wherever you see HVIO, you can transparently substite a regular Handle.
This module can work with any HVIO object, however.  See
"System.IO.HVIO" for more details.

Versions of MissingH prior 0.11.6 lacked the 'BinaryConvertible' class
and worked only with Strings and Handles.

Important note: /binary functions are not supported in all Haskell
implementations/.  Do not import or use this module unless you know you
are using an implementation that supports them.  At this time, here
is the support status:

 * GHC 6.2 and above: yes

 * GHC 6.x, earlier versions: unknown

 * GHC 5.x: no

 * nhc98: no

 * Hugs: partial (maybe complete; needs more testing)

Non-binary functions may be found in "System.IO".

See also: "System.IO.BlockIO"

Written by John Goerzen, jgoerzen\@complete.org
-}

module System.IO.Binary(
                       -- * Support for different types of blocks
                       BinaryConvertible(..),
                       -- * Entire File\/Handle Utilities
                       -- ** Opened Handle Data Copying
                       hBlockCopy, blockCopy,
                       -- ** Disk File Data Copying
                       copyFileBlocksToFile,
                       -- * Binary Single-Block I\/O
                       hPutBufStr, putBufStr, hGetBufStr, getBufStr,
                       hFullGetBufStr, fullGetBufStr,
                       -- * Binary Multi-Block I\/O
                       hGetBlocks, getBlocks, hFullGetBlocks, fullGetBlocks,
                       -- * Lazy Interaction
                       readBinaryFile, writeBinaryFile,
                       -- ** Binary Block-based
                       hBlockInteract, blockInteract,
                       hFullBlockInteract, fullBlockInteract
                        ) where

import Data.Word (Word8())
import Foreign.C.String (peekCStringLen, withCString)
import Foreign.C.Types (CChar())
import Foreign.ForeignPtr
import Foreign.Marshal.Array (peekArray, withArray)
import Foreign.Ptr
import System.IO
import System.IO.HVFS
import System.IO.HVIO
import System.IO.Unsafe (unsafeInterleaveIO)

{- | Provides support for handling binary blocks with convenient
types.

This module provides implementations for Strings and for [Word8] (lists of
Word8s). -}
class (Eq a, Show a) => BinaryConvertible a where
    toBuf :: [a] -> (Ptr CChar -> IO c) -> IO c
    fromBuf :: Int -> (Ptr CChar -> IO Int) -> IO [a]

instance BinaryConvertible Char where
    toBuf = withCString
    fromBuf len func =
        do fbuf <- mallocForeignPtrArray (len + 1)
           withForeignPtr fbuf handler
        where handler ptr =
                  do bytesread <- func ptr
                     peekCStringLen (ptr, bytesread)

instance BinaryConvertible Word8 where
    toBuf hslist func = withArray hslist (\ptr -> func (castPtr ptr))
    fromBuf len func =
        do (fbuf::(ForeignPtr Word8)) <- mallocForeignPtrArray (len + 1)
           withForeignPtr fbuf handler
        where handler ptr =
                  do bytesread <- func (castPtr ptr)
                     peekArray bytesread ptr


--  **************************************************
--  Binary Files
--  **************************************************

{- | As a wrapper around the standard function 'System.IO.hPutBuf',
this function takes a standard Haskell 'String' instead of the far less
convenient @Ptr a@.  The entire contents of the string will be written
as a binary buffer using 'hPutBuf'.  The length of the output will be
the length of the passed String or list.

If it helps, you can thing of this function as being of type
@Handle -> String -> IO ()@ -}
hPutBufStr :: (HVIO a, BinaryConvertible b) => a -> [b] -> IO ()
hPutBufStr f s = toBuf s (\cs -> vPutBuf f cs (length s))

-- | An alias for 'hPutBufStr' 'stdout'
putBufStr :: (BinaryConvertible b) => [b] -> IO ()
putBufStr = hPutBufStr stdout

{- | Acts a wrapper around the standard function 'System.IO.hGetBuf',
this function returns a standard Haskell String (or [Word8]) instead of
modifying
a 'Ptr a' buffer.  The length is the maximum length to read and the
semantice are the same as with 'hGetBuf'; namely, the empty string
is returned with EOF is reached, and any given read may read fewer
bytes than the given length.

(Actually, it's a wrapper around 'System.IO.HVIO.vGetBuf') -}
hGetBufStr :: (HVIO a, BinaryConvertible b) => a -> Int -> IO [b]
hGetBufStr f count = fromBuf count (\buf -> vGetBuf f buf count)

-- | An alias for 'hGetBufStr' 'stdin'
getBufStr :: (BinaryConvertible b) => Int -> IO [b]
getBufStr = hGetBufStr stdin

{- | Like 'hGetBufStr', but guarantees that it will only return fewer than
the requested number of bytes when EOF is encountered. -}
hFullGetBufStr :: (HVIO a, BinaryConvertible b) => a -> Int -> IO [b]
hFullGetBufStr _ 0 = return []
hFullGetBufStr f count = do
                         thisstr <- hGetBufStr f count
                         if thisstr == []
                            then return []
                            else do
                                 remainder <- hFullGetBufStr f (count - (length thisstr))
                                 return (thisstr ++ remainder)

-- | An alias for 'hFullGetBufStr' 'stdin'
fullGetBufStr :: BinaryConvertible b => Int -> IO [b]
fullGetBufStr = hFullGetBufStr stdin

{- | Writes the list of blocks to the given file handle -- a wrapper around
'hPutBufStr'.

Think of this function as:

>Handle -> [String] -> IO ()

(You can use it that way) -}
hPutBlocks :: (HVIO a, BinaryConvertible b) => a -> [[b]] -> IO ()
hPutBlocks _ [] = return ()
hPutBlocks h (x:xs) = do
                      hPutBufStr h x
                      hPutBlocks h xs

{- | An alias for 'hPutBlocks' 'stdout'
putBlocks :: (BinaryConvertible b) => [[b]] -> IO ()
putBlocks = hPutBlocks stdout -}

{- | Returns a lazily-evaluated list of all blocks in the input file,
as read by 'hGetBufStr'.  There will be no 0-length block in this list.
The list simply ends at EOF. -}
hGetBlocks :: (HVIO a, BinaryConvertible b) => a -> Int -> IO [[b]]
hGetBlocks = hGetBlocksUtil hGetBufStr

-- | An alias for 'hGetBlocks' 'stdin'
getBlocks :: BinaryConvertible b => Int -> IO [[b]]
getBlocks = hGetBlocks stdin

{- | Same as 'hGetBlocks', but using 'hFullGetBufStr' underneath. -}
hFullGetBlocks :: (HVIO a, BinaryConvertible b) => a -> Int -> IO [[b]]
hFullGetBlocks = hGetBlocksUtil hFullGetBufStr

-- | An alias for 'hFullGetBlocks' 'stdin'
fullGetBlocks :: BinaryConvertible b => Int -> IO [[b]]
fullGetBlocks = hFullGetBlocks stdin

hGetBlocksUtil :: (HVIO a, BinaryConvertible b) => (a -> Int -> IO [b]) -> a -> Int -> IO [[b]]
hGetBlocksUtil readfunc h count =
    unsafeInterleaveIO $ do
                       block <- readfunc h count
                       if block == []
                          then return []
                          else do
                               remainder <- hGetBlocksUtil readfunc h count
                               return (block : remainder)

{- | Binary block-based interaction.  This is useful for scenarios that
take binary blocks, manipulate them in some way, and then write them
out.  Take a look at 'hBlockCopy' for an example.  The integer argument
is the size of input binary blocks.  This function uses 'hGetBlocks'
internally.
-}
hBlockInteract :: (HVIO a, HVIO d, BinaryConvertible b, BinaryConvertible c) =>
                  Int -> a -> d -> ([[b]] -> [[c]]) -> IO ()
hBlockInteract = hBlockInteractUtil hGetBlocks

-- | An alias for 'hBlockInteract' over 'stdin' and 'stdout'
blockInteract :: (BinaryConvertible b, BinaryConvertible c) => Int -> ([[b]] -> [[c]]) -> IO ()
blockInteract x = hBlockInteract x stdin stdout

{- | Same as 'hBlockInteract', but uses 'hFullGetBlocks' instead of
'hGetBlocks' internally. -}
hFullBlockInteract :: (HVIO a, HVIO d, BinaryConvertible b, BinaryConvertible c) =>
                      Int -> a -> d -> ([[b]] -> [[c]]) -> IO ()
hFullBlockInteract = hBlockInteractUtil hFullGetBlocks

-- | An alias for 'hFullBlockInteract' over 'stdin' and 'stdout'
fullBlockInteract :: (BinaryConvertible b, BinaryConvertible c) =>
                     Int -> ([[b]] -> [[c]]) -> IO ()
fullBlockInteract x = hFullBlockInteract x stdin stdout

hBlockInteractUtil :: (HVIO a, HVIO d, BinaryConvertible b, BinaryConvertible c) =>
                      (a -> Int -> IO [[b]]) -> Int ->
                      a -> d -> ([[b]] -> [[c]]) -> IO ()
hBlockInteractUtil blockreader blocksize hin hout func =
    do
    blocks <- blockreader hin blocksize
    hPutBlocks hout (func blocks)

{- | Copies everything from the input handle to the output handle using binary
blocks of the given size.  This was once the following
beautiful implementation:

> hBlockCopy bs hin hout = hBlockInteract bs hin hout id

('id' is the built-in Haskell function that just returns whatever is given
to it)

In more recent versions of MissingH, it uses a more optimized routine that
avoids ever having to convert the binary buffer at all.
-}
hBlockCopy :: (HVIO a, HVIO b) => Int -> a -> b -> IO ()
hBlockCopy bs hin hout =
    do (fbuf::ForeignPtr CChar) <- mallocForeignPtrArray (bs + 1)
       withForeignPtr fbuf handler
    where handler ptr =
              do bytesread <- vGetBuf hin ptr bs
                 if bytesread > 0
                    then do vPutBuf hout ptr bytesread
                            handler ptr
                    else return ()

{- | Copies from 'stdin' to 'stdout' using binary blocks of the given size.
An alias for 'hBlockCopy' over 'stdin' and 'stdout'
-}
blockCopy :: Int -> IO ()
blockCopy bs = hBlockCopy bs stdin stdout

{- | Copies one filename to another in binary mode.

Please note that the Unix permission bits on the output file cannot
be set due to a limitation of the Haskell 'System.IO.openBinaryFile'
function.  Therefore, you may need to adjust those bits after the copy
yourself.

This function is implemented using 'hBlockCopy' internally. -}
copyFileBlocksToFile :: Int -> FilePath -> FilePath -> IO ()
copyFileBlocksToFile bs infn outfn = do
                                     hin <- openBinaryFile infn ReadMode
                                     hout <- openBinaryFile outfn WriteMode
                                     hBlockCopy bs hin hout
                                     hClose hin
                                     hClose hout
                                     return ()

{- | Like the built-in 'readFile', but opens the file in binary instead
of text mode. -}
readBinaryFile :: FilePath -> IO String
readBinaryFile = vReadBinaryFile SystemFS

{- | Same as 'readBinaryFile', but works with HVFS objects. -}
vReadBinaryFile :: (HVFSOpenable a) => a -> FilePath -> IO String
vReadBinaryFile fs fp =
    vOpenBinaryFile fs fp ReadMode >>= (\(HVFSOpenEncap h) -> vGetContents h)

{- | Like the built-in 'writeFile', but opens the file in binary instead
of text mode. -}
writeBinaryFile :: FilePath -> String -> IO ()
writeBinaryFile = vWriteBinaryFile SystemFS

{- | Like 'writeBinaryFile', but works on HVFS objects. -}
vWriteBinaryFile :: (HVFSOpenable a) => a -> FilePath -> String -> IO ()
vWriteBinaryFile fs name str =
    do h <- vOpenBinaryFile fs name WriteMode
       case h of
              HVFSOpenEncap x -> do vPutStr x str
                                    vClose x
