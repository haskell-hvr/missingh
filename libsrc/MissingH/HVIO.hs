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
                     StreamReader, newStreamReader,
                     MemoryVIO, newMemoryVIO,
                     PipeReader, PipeWriter, newPipe
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
    -- | Whether or not we're at EOF.  This may raise on exception
    -- on some items, most notably write-only Handles such as stdout.
    -- vIsEOF implementations must implicitly call vTestOpen.
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
    -- If an implementation overrides the default, make sure that it
    -- calls vTestOpen at some point.
    vTestEOF :: a -> IO ()

    vShow x = return (show x)

    vMkIOError _ et desc mfp =
        mkIOError et desc Nothing mfp

    vGetFP _ = return Nothing

    vThrow h et = do
                  fp <- vGetFP h
                  ioError (vMkIOError h et "" fp)

    vTestEOF h = do e <- vIsEOF h
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
        let loop accum = 
                let func = do c <- vGetChar h
                              case c of
                                     '\n' -> return accum
                                     x -> accum `seq` loop (accum ++ [x])
                    handler e = if isEOFError e then return accum
                                else ioError e
                    in catch func handler
            in
            do vGetChar h >>= \x -> loop [x]

    vGetContents h =
        let loop = 
                let func = do c <- vGetChar h
                              next <- loop
                              c `seq` return (c : next)
                    handler e = if isEOFError e then return []
                                else ioError e
                    in catch func handler
            in
            do firstchar <- vGetChar h
               rest <- loop
               return (firstchar : rest)
           
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
    -- | Flush any output buffers.
    -- Note: implementations should assure that a vFlush is performed
    -- on file close, if necessary to ensure all data sent is written.
    vFlush :: a -> IO ()

    vPutStr _ [] = return ()
    vPutStr h (x:xs) = do vPutChar h x
                          vPutStr h xs

    vPutStrLn h s = vPutStr h (s ++ "\n")

    vPrint h s = vPutStrLn h (show s)
                 
    vFlush = vTestOpen

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
    vFlush = hFlush

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
vioc_close x = modifyIORef x (\ (_, dat) -> (False, dat))

vioc_set :: VIOCloseSupport a -> a -> IO ()
vioc_set x newdat = modifyIORef x (\ (stat, _) -> (stat, newdat))

----------------------------------------------------------------------
-- Stream Readers
----------------------------------------------------------------------

{- | Simulate I\/O based on a string buffer.

This is lazy!
 -}
newtype StreamReader = StreamReader (VIOCloseSupport String)

newStreamReader :: String -> IO StreamReader
newStreamReader s = do ref <- newIORef (True, s)
                       return (StreamReader ref)

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

----------------------------------------------------------------------
-- Buffers
----------------------------------------------------------------------

{- | Simulate true I\/O on a buffer.

-}
newtype MemoryVIO = MemoryVIO (VIOCloseSupport (Int, String))

newMemoryVIO :: IO MemoryVIO
newMemoryVIO = do ref <- newIORef (True, (0, ""))
                  return (MemoryVIO ref)

vrv (MemoryVIO x) = x

-- | Grab the entire contents of the buffer as a string.
getMemoryVIOBuffer :: MemoryVIO -> IO String
getMemoryVIOBuffer h = do c <- vioc_get (vrv h)
                          return (snd c)

instance Show MemoryVIO where
    show _ = "<MemoryVIO>"

instance HVIOGeneric MemoryVIO where
    vClose = vioc_close . vrv
    vIsEOF h = do vTestOpen h
                  c <- vioc_get (vrv h)
                  return ((length (snd c)) == (fst c))
    vIsOpen = vioc_isopen . vrv

instance HVIOReader MemoryVIO where
    vGetChar h = do vTestEOF h
                    c <- vioc_get (vrv h)
                    let retval = (snd c) !! (fst c)
                    vioc_set (vrv h) (succ (fst c), snd c)
                    return retval
    vGetContents h = do vTestEOF h
                        v <- vioc_get (vrv h)
                        let retval = drop (fst v) (snd v)
                        vioc_set (vrv h) (-1, "")
                        vClose h
                        return retval

instance HVIOWriter MemoryVIO where
    vPutStr h s = do (pos, buf) <- vioc_get (vrv h)
                     let (pre, post) = splitAt pos buf
                     let newbuf = pre ++ s ++ (drop (length buf) post)
                     vioc_set (vrv h) (pos + (length buf), newbuf)
    vPutChar h c = vPutStr h [c]

instance HVIOSeeker MemoryVIO where
    vTell h = do v <- vioc_get (vrv h)
                 return . fromIntegral $ (fst v)
    vSeek h seekmode seekposp = 
        do (pos, buf) <- vioc_get (vrv h)
           let seekpos = fromInteger seekposp 
           let newpos = case seekmode of
                             AbsoluteSeek -> seekpos
                             RelativeSeek -> pos + seekpos
                             SeekFromEnd -> (length buf) + seekpos
           let buf2 = buf ++ if newpos > (length buf)
                                then replicate (newpos - (length buf)) '\0'
                                else []
           vioc_set (vrv h) (newpos, buf2)

----------------------------------------------------------------------
-- Pipes
----------------------------------------------------------------------

newPipe :: IO (PipeReader, PipeWriter)
newPipe = do mv <- newEmptyMVar
             readerref <- newIORef (True, mv)
             let reader = PipeReader readerref
             writerref <- newIORef (True, reader)
             return (reader, PipeWriter writerref)

data PipeBit = PipeBit Char 
             | PipeEOF
               deriving (Eq, Show)

newtype PipeReader = PipeReader (VIOCloseSupport (MVar PipeBit))
newtype PipeWriter = PipeWriter (VIOCloseSupport PipeReader)

------------------------------
-- Pipe Reader
------------------------------

prv (PipeReader x) = x

instance Show PipeReader where
    show x = "<PipeReader>"

instance HVIOGeneric PipeReader where
    vClose = vioc_close . prv
    vIsOpen = vioc_isopen . prv
    vIsEOF h = do vTestOpen h
                  mv <- vioc_get (prv h)
                  dat <- readMVar mv
                  return (dat == PipeEOF)

pr_getc h = do mv <- vioc_get (prv h)
               takeMVar mv

instance HVIOReader PipeReader where
    vGetChar h = do vTestEOF h
                    c <- pr_getc h
                    case c of 
                        PipeBit x -> return x
                        -- vTestEOF should eliminate this case
                        _ -> fail "Internal error in HVIOReader vGetChar"
    vGetContents h = 
        let loop = do c <- pr_getc h
                      case c of
                          PipeEOF -> return []
                          PipeBit x -> do next <- loop
                                          return (x : next)
        in do vTestEOF h
              loop
                        
------------------------------
-- Pipe Writer
------------------------------

pwv (PipeWriter x) = x
pwmv (PipeWriter x) = do mv1 <- vioc_get x
                         vioc_get (prv mv1)


instance Show PipeWriter where
    show x = "<PipeWriter>"

instance HVIOGeneric PipeWriter where
    vClose h = do o <- vIsOpen h
                  if o then do
                            mv <- pwmv h
                            putMVar mv PipeEOF
                            vioc_close (pwv h)
                     else return ()
    vIsOpen = vioc_isopen . pwv
    vIsEOF h = do vTestOpen h
                  return False

instance HVIOWriter PipeWriter where
    -- FIXME: race condition below (could be closed after testing)
    vPutChar h c = do vTestOpen h
                      child <- vioc_get (pwv h)
                      copen <- vIsOpen child
                      if copen 
                         then do mv <- pwmv h
                                 putMVar mv (PipeBit c)
                         else fail "PipeWriter: Couldn't write to pipe because child end is closed"
