{- arch-tag: I/O utilities main file
Copyright (C) 2004 John Goerzen <jgoerzen@complete.org>

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

{- | This module provides various helpful utilities for dealing with I\/O.

Written by John Goerzen, jgoerzen\@complete.org
-}

module MissingH.IOutil(-- * Entire File\/Handle Utilities
                       -- ** Opened Handle Data Copying
                       hBlockCopy, blockCopy,
                       -- ** Disk File Data Copying
                       -- * Line Processing Utilities
                       hPutStrLns, hGetLines,
                       -- * Binary Single-Block I\/O
                       hPutBufStr, putBufStr, hGetBufStr, getBufStr,
                       hFullGetBufStr, fullGetBufStr,
                       -- * Binary Multi-Block I\/O
                       hGetBlocks, getBlocks, hFullGetBlocks, fullGetBlocks,
                       -- * Lazy Interaction
                       -- ** Character-based
                       hInteract,
                       -- ** Line-based
                       hLineInteract, lineInteract,
                       -- ** Binary Block-based
                       hBlockInteract, blockInteract,
                       hFullBlockInteract, fullBlockInteract
                        ) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
import System.IO.Unsafe
import System.IO

{- | Given a list of strings, output a line containing each item, adding
newlines as appropriate.  The list is not expected to have newlines already.
-}

hPutStrLns :: Handle -> [String] -> IO ()
hPutStrLns _ [] = return ()
hPutStrLns h (x:xs) = do
                      hPutStrLn h x
                      hPutStrLns h xs

{- | Given a handle, returns a list of all the lines in that handle.
Thanks to lazy evaluation, this list does not have to be read all at once.

Combined with 'hPutStrLns', this can make a powerful way to develop
filters.  See the 'lineInteract' function for more on that concept.

Example:

> main = do
>        l <- hGetLines stdin
>        hPutStrLns stdout $ filter (startswith "1") l

-}

hGetLines :: Handle -> IO [String]

hGetLines h = unsafeInterleaveIO (do
                                  ieof <- hIsEOF h
                                  if (ieof) 
                                     then return []
                                     else do
                                          line <- hGetLine h
                                          remainder <- hGetLines h
                                          return (line : remainder)
                                 )


{- | This is similar to the built-in 'System.IO.interact', but works
on any handle, not just stdin and stdout.

In other words:

> interact = hInteract stdin stdout
-}
hInteract :: Handle -> Handle -> (String -> String) -> IO ()
hInteract finput foutput func = do
                                content <- hGetContents finput
                                hPutStr stdout (func content)

{- | Line-based interaction.  This is similar to wrapping your
interact functions with 'lines' and 'unlines'.  This equality holds:

> lineInteract = 'hLineInteract' stdin stdout

Here's an example:

> main = lineInteract (filter (startswith "1"))
-}
lineInteract :: ([String] -> [String]) -> IO ()
lineInteract = hLineInteract stdin stdout

{- | Line-based interaction over arbitrary handles.  This is similar
to wrapping hInteract with 'lines' and 'unlines'.

One could view this function like this:

> hLineInteract finput foutput func = 
>     let newf = unlines . func . lines in
>         hInteract finput foutput newf

Though the actual implementation is this for efficiency:

> hLineInteract finput foutput func =
>     do
>     lines <- hGetLines finput
>     hPutStrLns foutput (func lines)
-}

hLineInteract :: Handle -> Handle -> ([String] -> [String]) -> IO ()
hLineInteract finput foutput func =
    do
    lines <- hGetLines finput
    hPutStrLns foutput (func lines)

-- . **************************************************
-- . Binary Files
-- . **************************************************


{- | As a wrapper around the standard function 'System.IO.hPutBuf',
this function takes a standard Haskell 'String' instead of the far less
convenient 'Ptr a'.  The entire contents of the string will be written
as a binary buffer using 'hPutBuf'.  The length of the output will be
the length of the string. -}
hPutBufStr :: Handle -> String -> IO ()
hPutBufStr f s = withCString s (\cs -> hPutBuf f cs (length s))

-- | An alias for 'hPutBufStr' 'stdout'
putBufStr :: String -> IO ()
putBufStr = hPutBufStr stdout

{- | As a wrapper around the standard function 'System.IO.hGetBuf',
this function returns a standard Haskell string instead of modifying
a 'Ptr a' buffer.  The length is the maximum length to read and the
semantice are the same as with 'hGetBuf'; namely, the empty string
is returned with EOF is reached, and any given read may read fewer
bytes than the given length. -}
hGetBufStr :: Handle -> Int -> IO String
hGetBufStr f count = do
   fbuf <- mallocForeignPtrArray (count + 1)
   withForeignPtr fbuf (\buf -> do
                        bytesread <- hGetBuf f buf count
                        haskstring <- peekCStringLen (buf, bytesread)
                        return haskstring)

-- | An alias for 'hGetBufStr' 'stdin'
getBufStr :: Int -> IO String
getBufStr = hGetBufStr stdin

{- | Like 'hGetBufStr', but guarantees that it will only return fewer than
the requested number of bytes when EOF is encountered. -}
hFullGetBufStr :: Handle -> Int -> IO String
hFullGetBufStr f 0 = return ""
hFullGetBufStr f count = do
                         thisstr <- hGetBufStr f count
                         if thisstr == "" -- EOF
                            then return ""
                            else do
                                 remainder <- hFullGetBufStr f (count - (length thisstr))
                                 return (thisstr ++ remainder)

-- | An alias for 'hFullGetBufStr' 'stdin'
fullGetBufStr :: Int -> IO String
fullGetBufStr = hFullGetBufStr stdin

{- | Writes the list of blocks to the given file handle -- a wrapper around
'hPutBufStr'. -}
hPutBlocks :: Handle -> [String] -> IO ()
hPutBlocks _ [] = return ()
hPutBlocks h (x:xs) = do
                      hPutBufStr h x
                      hPutBlocks h xs

-- | An alias for 'hPutBlocks' 'stdout'
putBlocks :: [String] -> IO ()
putBlocks = hPutBlocks stdout

{- | Returns a lazily-evaluated list of all blocks in the input file,
as read by 'hGetBufStr'.  There will be no 0-length block in this list.
The list simply ends at EOF. -}
hGetBlocks :: Handle -> Int -> IO [String]
hGetBlocks = hGetBlocksUtil hGetBufStr

-- | An alias for 'hGetBlocks' 'stdin'
getBlocks :: Int -> IO [String]
getBlocks = hGetBlocks stdin

{- | Same as 'hGetBlocks', but using 'hFullGetBufStr' underneath. -}
hFullGetBlocks :: Handle -> Int -> IO [String]
hFullGetBlocks = hGetBlocksUtil hFullGetBufStr

-- | An alias for 'hFullGetBlocks' 'stdin'
fullGetBlocks :: Int -> IO [String]
fullGetBlocks = hFullGetBlocks stdin

hGetBlocksUtil :: (Handle -> Int -> IO String) -> Handle -> Int -> IO [String]
hGetBlocksUtil readfunc h count =
    unsafeInterleaveIO (do
                       block <- readfunc h count
                       if block == ""
                          then return []
                          else do
                               remainder <- hGetBlocksUtil readfunc h count
                               return (block : remainder)
                       )

{- | Binary block-based interaction.  This is useful for scenarios that
take binary blocks, manipulate them in some way, and then write them
out.  Take a look at 'hBlockCopy' for an example.  The integer argument
is the size of input binary blocks.  This function uses 'hGetBlocks'
internally.
-}
hBlockInteract :: Int -> Handle -> Handle -> ([String] -> [String]) -> IO ()
hBlockInteract = hBlockInteractUtil hGetBlocks

-- | An alias for 'hBlockInteract' over 'stdin' and 'stdout'
blockInteract :: Int -> ([String] -> [String]) -> IO ()
blockInteract x = hBlockInteract x stdin stdout

{- | Same as 'hBlockInteract', but uses 'hFullGetBlocks' instead of
'hGetBlocks' internally. -}
hFullBlockInteract :: Int -> Handle -> Handle -> ([String] -> [String]) -> IO ()
hFullBlockInteract = hBlockInteractUtil hFullGetBlocks

-- | An alias for 'hFullBlockInteract' over 'stdin' and 'stdout'
fullBlockInteract :: Int -> ([String] -> [String]) -> IO ()
fullBlockInteract x = hFullBlockInteract x stdin stdout

hBlockInteractUtil :: (Handle -> Int -> IO [String]) -> Int ->
                      Handle -> Handle -> ([String] -> [String]) -> IO ()
hBlockInteractUtil blockreader blocksize hin hout func =
    do
    blocks <- blockreader hin blocksize
    hPutBlocks hout (func blocks)

{- | Copies everything from the input handle to the output handle using binary
blocks of the given size.  This is actually a beautiful implementation:

> hBlockCopy bs hin hout = 'hBlockInteract' bs hin hout id

('id' is the built-in Haskell function that just returns whatever is given
to it)
-}

hBlockCopy :: Int -> Handle -> Handle -> IO ()
hBlockCopy bs hin hout = hBlockInteract bs hin hout id

{- | Copies from stdin to stdout using binary blocks of the given size.
-}
blockCopy :: Int -> IO ()
blockCopy bs = hBlockCopy bs stdin stdout
