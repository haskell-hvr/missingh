{- arch-tag: HVIO main file
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

{- |
   Module     : MissingH.HVIO
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Haskell Virtual I\/O system main file

Copyright (c) 2004 John Goerzen, jgoerzen\@complete.org

-}

module MissingH.HVIO(-- * Implementation Classes
                     HVIOGeneric(..), 
                     HVIOReader(..),
                     HVIOWriter(..),
                     HVIOSeeker(..),
                     -- * Standard Virtual IO features
                     -- | Note: Handle is a member of all classes by default.
                     StreamReader
                    )
where

import System.IO
import System.IO.Error
import Control.Concurrent.MVar
import Data.IORef

{- | The HVIOGeneric class.

Implementators must provide 'vClose', 'vIsEOF', and either
'vIsOpen' or 'vIsClosed'. -}
class (Show a) => HVIOGeneric a where
    -- | Close a file
    vClose :: a -> IO ()
    -- | Test if a file is open
    vIsOpen :: a -> IO Bool
    -- | Test if a file is closed
    vIsClosed :: a -> IO Bool
    -- | Raise an error if the file is not open.
    vTestOpen :: a -> IO ()
    -- | Whether or not we're at EOF
    vIsEOF :: a -> IO Bool
    -- | Detailed show output.
    vShow :: a -> IO String
    -- | Make an IOError.
    vMkIOError :: a -> IOErrorType -> String -> Maybe FilePath -> IOError
    -- | Throw an IOError.
    vThrow :: a -> IOErrorType -> IO b
    -- | Get the filename\/object\/whatever that this corresponds to.
    -- May be Nothing.
    vGetFP :: a -> IO (Maybe FilePath)
    -- | Throw an isEOFError if we're at EOF; returns nothing otherwise.
    -- vTestEOF will implicitly call vTestOpen.
    vTestEOF :: a -> IO ()

    vShow x = return (show x)

    vMkIOError _ et desc mfp =
        mkIOError et desc Nothing mfp

    vGetFP _ = return Nothing

    vThrow h et = do
                  fp <- vGetFP h
                  ioError (vMkIOError h et "" fp)

    vTestEOF h = do vTestOpen h
                    e <- vIsEOF h
                    if e then vThrow h eofErrorType
                       else return ()

    vIsOpen h = vIsClosed h >>= return . not
    vIsClosed h = vIsOpen h >>= return . not
    vTestOpen h = do e <- vIsClosed h
                     if e then vThrow h illegalOperationErrorType
                        else return ()

{- | Readers.  Implementators must provide at least 'vGetChar'.
An implementation of 'vGetContents' is also highly suggested, since
the default cannot implement quick closing.
-}
class (HVIOGeneric a) => HVIOReader a where
    -- | Read one character
    vGetChar :: a -> IO Char
    -- | Read one line
    vGetLine :: a -> IO String
    -- | Get the remaining contents.  
    vGetContents :: a -> IO String
    -- | Indicate whether at least one item is ready for reading.
    vReady :: a -> IO Bool

    vGetLine h = 
        let loop accum = do e <- vIsEOF h
                            if e then return accum
                               else do c <- vGetChar h
                                       case c of
                                           '\n' -> return accum
                                           x -> accum `seq` loop (accum ++ [x])
            in
            do vTestEOF h
               loop ""

    vGetContents h =
        let loop = do e <- vIsEOF h
                      if e then return []
                         else do c <- vGetChar h
                                 next <- loop
                                 c `seq` return (c : next)
            in
            do vTestEOF h
               loop
           
    vReady h = do vTestEOF h
                  return True

{- | Writers.  Implementators must provide at least 'vPutChar'. -}

class (HVIOGeneric a) => HVIOWriter a where
    -- | Write one character
    vPutChar :: a -> Char -> IO ()
    -- | Write a string
    vPutStr :: a -> String -> IO ()
    -- | Write a string with newline character after it
    vPutStrLn :: a -> String -> IO ()
    -- | Write a string representation of the argument, plus a newline.
    vPrint :: Show b => a -> b -> IO ()

    vPutStr _ [] = return ()
    vPutStr h (x:xs) = do vPutChar h x
                          vPutStr h xs

    vPutStrLn h s = vPutStr h (s ++ "\n")

    vPrint h s = vPutStrLn h (show s)

{- | Seekable items.  Implementators must provide all functions.

-}

class (HVIOGeneric a) => HVIOSeeker a where
    -- | Seek to a specific location.
    vSeek :: a -> SeekMode -> Integer -> IO ()

    -- | Get the current position.
    vTell :: a -> IO Integer

----------------------------------------------------------------------
-- Handle instances
----------------------------------------------------------------------

instance HVIOGeneric Handle where
    vClose = hClose
    vIsEOF = hIsEOF
    vShow = hShow
    vMkIOError h et desc mfp =
        mkIOError et desc (Just h) mfp

instance HVIOReader Handle where
    vGetChar = hGetChar
    vGetLine = hGetLine
    vGetContents = hGetContents
    vReady = hReady

instance HVIOWriter Handle where
    vPutChar = hPutChar
    vPutStr = hPutStr
    vPutStrLn = hPutStrLn
    vPrint = hPrint

instance HVIOSeeker Handle where
    vSeek = hSeek
    vTell = hTell

----------------------------------------------------------------------
-- VIO Support
----------------------------------------------------------------------
type VIOCloseSupport a = IORef (Bool, a)

vioc_isopen :: VIOCloseSupport a -> IO Bool
vioc_isopen x = readIORef x >>= return . fst

vioc_get :: VIOCloseSupport a -> IO a
vioc_get x = readIORef x >>= return . snd

vioc_close :: VIOCloseSupport a -> IO ()
vioc_close x = modifyIORef x (\ _ -> (False, undefined))

vioc_set :: VIOCloseSupport a -> a -> IO ()
vioc_set x newdat = modifyIORef x (\ (stat, _) -> (stat, newdat))

----------------------------------------------------------------------
-- Stream Readers/Writers
----------------------------------------------------------------------

{- | Simulate I\/O based on a string buffer.

This is lazy!
 -}
newtype StreamReader = StreamReader (VIOCloseSupport String)

srv (StreamReader x) = x
instance Show StreamReader where
    show _ = "<StreamReader>"

instance HVIOGeneric StreamReader where
    vClose = vioc_close . srv
    vIsEOF h = do vTestOpen h
                  d <- vioc_get (srv h)
                  return $ case d of
                                  [] -> True
                                  _ -> False
    vIsOpen = vioc_isopen . srv

instance HVIOReader StreamReader where
    vGetChar h = do vTestEOF h
                    c <- vioc_get (srv h)
                    let retval = head c
                    vioc_set (srv h) (tail c)
                    return retval
    
    vGetContents h = do vTestEOF h
                        c <- vioc_get (srv h)
                        vClose h
                        return c

