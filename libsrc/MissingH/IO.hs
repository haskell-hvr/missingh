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

There are more functions in "MissingH.IO.Binary".

Written by John Goerzen, jgoerzen\@complete.org
-}

module MissingH.IO(-- * Entire File\/Handle Utilities
                       -- ** Opened Handle Data Copying
                       hLineCopy, lineCopy,
                       -- ** Disk File Data Copying
                       copyFileLinesToFile,
                       -- * Line Processing Utilities
                       hPutStrLns, hGetLines,
                       -- * Lazy Interaction
                       -- ** Character-based
                       hInteract,
                       -- ** Line-based
                       hLineInteract, lineInteract,
                        ) where

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

> lineInteract = hLineInteract stdin stdout

Here's an example:

> main = lineInteract (filter (startswith "1"))

This will act as a simple version of grep -- all lines that start with 1
will be displayed; all others will be ignored.
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

{- | Copies from one handle to another in text mode (with lines).
Like 'hBlockCopy', this implementation is nice:

> hLineCopy hin hout = hLineInteract hin hout id
-}

hLineCopy :: Handle -> Handle -> IO()
hLineCopy hin hout = hLineInteract hin hout id

{- | Copies from 'stdin' to 'stdout' using lines.  An alias for 'hLineCopy'
over 'stdin' and 'stdout'. -}

lineCopy :: IO ()
lineCopy = hLineCopy stdin stdout

{- | Copies one filename to another in text mode.

Please note that the Unix permission bits are set at a default; you may
need to adjust them after the copy yourself.

This function is implemented using 'hLineCopy' internally. -}

copyFileLinesToFile :: FilePath -> FilePath -> IO ()
copyFileLinesToFile infn outfn = do
                                 hin <- openFile infn ReadMode
                                 hout <- openFile outfn WriteMode
                                 hLineCopy hin hout
                                 hClose hin
                                 hClose hout
                                 return ()
                                 

