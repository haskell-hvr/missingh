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
   Module     : MissingH.IO.HVIO
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Haskell Virtual I\/O -- a system to increase the flexibility of input and
output in Haskell

Copyright (c) 2004 John Goerzen, jgoerzen\@complete.org

HVIO provides the following general features:

 * The ability to use a single set of functions on various different
   types of objects, including standard Handles, in-memory buffers,
   compressed files, network data streams, etc.

 * The ability to transparently add filters to the I\/O process.
   These filters could include things such as character set conversions,
   compression or decompression of a data stream, and more.

 * The ability to define new objects that have the properties
   of I\/O objects and can be used interchangably with them.

 * Specification compatibility with, and complete support for,
   existing I\/O on Handles.

 * Provide easier unit testing capabilities for I\/O actions

HVIO defines several basic type classes that you can use.  You will mostly
be interested in 'HVIOReader', 'HVIOWriter', and 'HVIOSeeker'.  Objects that
support reading will be an instance of 'HVIOReader'; those that suport writing,
'HVIOWriter'; and those that support seeking (random access), 'HVIOSeeker'.
Some, such as Handle, will be an instance of all of these.

It's trivial to adapt old code to work with HVIO.  For instance, consider
this example of old and new code:

>printMsg :: Handle -> String -> IO ()
>printMsg h msg = hPutStr h ("msg: " ++ msg)

And now, the new way:

>printMsg :: HVIOWriter h => h -> String -> IO ()
>printMsg h msg = vPutStr h ("msg: " ++ msg)

There are several points to note about this conversion:

 * The new method can still accept a Handle in exactly the same way as
   the old method.  Changing your functions to use HVIO will require no
   changes from functions that call them with Handles.

 * Most \"h\" functions have equivolent \"v\" functions that operate
   on HVIO classes instead of the more specific Handle.  The \"v\" functions
   behave identically to the \"h\" functions whenever possible.

 * There is no equivolent of \"openFile\" in any HVIO class.  You must
   create your Handle (or other HVIO object) using normal means.
   This is because the creation is so different that it cannot be standardized.

In addition to Handle, there are several pre-defined classes for your use.
'StreamReader' is a particularly interesting one.  At creation time, you pass
it a String.  Its contents are read lazily whenever a read call is made.  It
can be used, therefore, to implement filters (simply initialize it with the
result from, say, a map over hGetContents from another HVIO object), codecs,
and simple I\/O testing.  Because it is lazy, it need not hold the entire
string in memory.  You can create a 'StreamReader' with a call to
'newStreamReader'.

'MemoryBuffer' is a similar class, but with a different purpose.  It provides
a full interface like Handle (it implements 'HVIOReader', 'HVIOWriter',
and 'HVIOSeeker').  However, it maintains an in-memory buffer with the 
contents of the file, rather than an actual on-disk file.  You can access
the entire contents of this buffer at any time.  This can be quite useful
for testing I\/O code, or for cases where existing APIs use I\/O, but you
prefer a String representation.  You can create a 'MemoryBuffer' with a call
to 'newMemoryBuffer'.

Finally, there are pipes.  These pipes are analogous to the Unix
pipes that are available from System.Posix, but don't require Unix and work
only in Haskell.  When you create a pipe, you actually get two HVIO objects:
a 'PipeReader' and a 'PipeWriter'.  You must use the 'PipeWriter' in one
thread and the 'PipeReader' in another thread.  Data that's written to the
'PipeWriter' will then be available for reading with the 'PipeReader'.  The
pipes are implemented completely with existing Haskell threading primitives,
and require no special operating system support.  Unlike Unix pipes, these
pipes cannot be used across a fork().  Also unlike Unix pipes, these pipes
are portable and interact well with Haskell threads.  A new pipe can be created
with a call to 'newHVIOPipe'.

-}

module MissingH.IO.HVIO(-- * Implementation Classes
                     HVIOGeneric(..), 
                     HVIOReader(..),
                     HVIOWriter(..),
                     HVIOSeeker(..),
                     -- * Standard HVIO Implementations

                     -- ** Handle
                     -- | Handle is a member of all four classes.

                     -- ** Stream Reader
                     StreamReader, newStreamReader,

                     -- ** Memory Buffer
                     MemoryBuffer, newMemoryBuffer, getMemoryBuffer,

                     -- ** Haskell Pipe
                     PipeReader, PipeWriter, newHVIOPipe
                    )
where

import System.IO
import System.IO.Error
import Control.Concurrent.MVar
import Data.IORef

{- | This is the generic I\/O support class.  All objects that are to be used
in the HVIO system must provide an instance of 'HVIOGeneric'.

Functions in this class provide an interface with the same specification as
the similar functions in System.IO.  Please refer to that documentation
for a more complete specification than is provided here.

Instances of 'HVIOGeneric' must provide 'vClose', 'vIsEOF', and either
'vIsOpen' or 'vIsClosed'.
-}
class (Show a) => HVIOGeneric a where
    -- | Close a file
    vClose :: a -> IO ()
    -- | Test if a file is open
    vIsOpen :: a -> IO Bool
    -- | Test if a file is closed
    vIsClosed :: a -> IO Bool
    -- | Raise an error if the file is not open.
    -- This is a new HVIO function and is implemented in terms of
    -- 'vIsOpen'.
    vTestOpen :: a -> IO ()
    -- | Whether or not we're at EOF.  This may raise on exception
    -- on some items, most notably write-only Handles such as stdout.
    -- In general, this is most reliable on items opened for reading.
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
    -- calls vTestOpen at some point.  The default implementation is
    -- a wrapper around a call to 'vIsEOF'.
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

{- | This class defines reading functions.  Any object that can provide
reading capabilities should define an instance of this type class.

Functions in this class provide an interface with the same specification as
the similar functions in System.IO.  Please refer to that documentation
for a more complete specification than is provided here.

Implementators must provide at least 'vGetChar'.
An implementation of 'vGetContents' is also highly suggested, since
the default cannot implement proper partial closing semantics.

Being a member of the 'HVIOReader' class only means that an object, in general,
has reading capabilities -- and not that any particular one supports reading.
For instance, Handle implements HVIOReader, but a Handle could be open
write-only, and thus read attempts on it would fail.  You may use 'vIsReadable'
to ensure that a particular instance is open for reading.
-}
class (HVIOGeneric a) => HVIOReader a where
    -- | Read one character
    vGetChar :: a -> IO Char
    -- | Read one line
    vGetLine :: a -> IO String
    {- | Get the remaining contents.  Please note that as a user of this
       function, the same partial-closing semantics as are used in the
       standard 'hGetContents' are /encouraged/ from implementators,
       but are not /required/.  That means that, for instance,
       a 'vGetChar' after a 'vGetContents' may return some undefined
       result instead of the error you would normally get.  You should
       use caution to make sure your code doesn't fall into that trap,
       or make sure to test your code with Handle or one of the 
       default instances defined in this module.  Also, some implementations
       may essentially provide a complete close after a call to 'vGetContents'.
       The bottom line: after a call to 'vGetContents', you should do nothing
       else with the object save closing it with 'vClose'.
       
       For implementators, you are highly encouraged to provide a correct
       implementation. -}
    vGetContents :: a -> IO String
    -- | Indicate whether at least one item is ready for reading.
    -- This will always be True for a great many implementations.
    vReady :: a -> IO Bool
    -- | Indicate whether a particular item is available for reading.
    vIsReadable :: a -> IO Bool

    vIsReadable _ = return True

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

{- | Objects that implement 'HVIOWriter' provide writing capabilities.
Any object that can handle output should define an instance of this class.

Functions in this class provide an interface with the same specification as
the similar functions in System.IO.  Please refer to that documentation
for a more complete specification than is provided here.

Implementators of 'HVIOWriter' objects must provide at least 'vPutChar'.

Being a member of the 'HVIOWriter' class only means that an object, in general,
has writing capabilities -- and not that any particular one supports writing.
For instance, Handle implements HVIOWriter, but a Handle could be open
read-only, and thus write attempts on it would fail.  You may use 'vIsWritable'
to ensure that a particular instance is open for reading.
 -}

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
    -- Note: implementations should assure that a vFlush is automatically 
    -- performed
    -- on file close, if necessary to ensure all data sent is written.
    vFlush :: a -> IO ()
    -- | Indicate whether or not this particular object supports writing.
    vIsWritable :: a -> IO Bool

    vIsWritable _ = return True

    vPutStr _ [] = return ()
    vPutStr h (x:xs) = do vPutChar h x
                          vPutStr h xs

    vPutStrLn h s = vPutStr h (s ++ "\n")

    vPrint h s = vPutStrLn h (show s)
                 
    vFlush = vTestOpen

{- | This class defines seekable (random-access) objects.  Anything that is
a member of this class can have its file pointer repositioned forwards or
backwards.

Implementators must provide at least 'vTell' and 'vSeek'.

Functions in this class provide an interface with the same specification as
the similar functions in System.IO.  Please refer to that documentation
for a more complete specification than is provided here.

Being a member of the 'HVIOSeeker' class only means that an object, in general,
has seeker capabilities -- and not that any particular one supports writing.
For instance, Handle implements HVIOSeeker, but a Handle could be open
on a terminal device, and thus seek attempts on it would fail.  You may use
'vIsSeekable' to ensure that a particular instance supports seeking.

-}

class (HVIOGeneric a) => HVIOSeeker a where
    -- | Seek to a specific location.
    vSeek :: a -> SeekMode -> Integer -> IO ()

    -- | Get the current position.
    vTell :: a -> IO Integer

    -- | Convenience function to reset the file pointer to the beginning
    -- of the file.  A call to @vRewind h@ is the
    -- same as @'vSeek' h AbsoluteSeek 0@.
    vRewind :: a -> IO ()

    -- | Indicate whether this instance supports seeking.
    vIsSeekable :: a -> IO Bool

    vIsSeekable _ = return True

    vRewind h = vSeek h AbsoluteSeek 0

----------------------------------------------------------------------
-- Handle instances
----------------------------------------------------------------------

-- | FOO2
instance HVIOGeneric Handle where
    vClose = hClose
    vIsEOF = hIsEOF
    --vShow = hShow
    vMkIOError h et desc mfp =
        mkIOError et desc (Just h) mfp

instance HVIOReader Handle where
    vGetChar = hGetChar
    vGetLine = hGetLine
    vGetContents = hGetContents
    vReady = hReady
    vIsReadable = hIsReadable

instance HVIOWriter Handle where
    vPutChar = hPutChar
    vPutStr = hPutStr
    vPutStrLn = hPutStrLn
    vPrint = hPrint
    vFlush = hFlush
    vIsWritable = hIsWritable

instance HVIOSeeker Handle where
    vSeek = hSeek
    vTell = hTell
    vIsSeekable = hIsSeekable

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

When a 'StreamReader' is created, it is initialized based on the contents of
a 'String'.  Its contents are read lazily whenever a request is made to read
something from the 'StreamReader'.    It
can be used, therefore, to implement filters (simply initialize it with the
result from, say, a map over hGetContents from another HVIO object), codecs,
and simple I\/O testing.  Because it is lazy, it need not hold the entire
string in memory.  You can create a 'StreamReader' with a call to
'newStreamReader'.
 -}
newtype StreamReader = StreamReader (VIOCloseSupport String)

{- | Create a new 'StreamReader' object. -}
newStreamReader :: String            -- ^ Initial contents of the 'StreamReader'
                -> IO StreamReader
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

{- | A 'MemoryBuffer' simulates true I\/O, but uses an in-memory buffer instead
of on-disk storage.


 It provides
a full interface like Handle (it implements 'HVIOReader', 'HVIOWriter',
and 'HVIOSeeker').  However, it maintains an in-memory buffer with the 
contents of the file, rather than an actual on-disk file.  You can access
the entire contents of this buffer at any time.  This can be quite useful
for testing I\/O code, or for cases where existing APIs use I\/O, but you
prefer a String representation.  You can create a 'MemoryBuffer' with a call
to 'newMemoryBuffer'.

The present 'MemoryBuffer' implementation is rather inefficient, particularly
when reading towards the end of large files.  It's best used for smallish
data storage.  This problem will be fixed eventually.
-}
newtype MemoryBuffer = MemoryBuffer (VIOCloseSupport (Int, String))

{- | Create a new 'MemoryBuffer' instance.  The buffer is initially empty;
you can put things in it by using the normal 'vPutStr' calls, and reset to
the beginning by using the normal 'vRewind' call. -}
newMemoryBuffer :: IO MemoryBuffer
newMemoryBuffer = do ref <- newIORef (True, (0, ""))
                     return (MemoryBuffer ref)

vrv (MemoryBuffer x) = x

{- | Grab the entire contents of the buffer as a string. 
Unlike 'vGetContents', this has no effect on the open status of the
item, the EOF status, or the current position of the file pointer. -}
getMemoryBuffer :: MemoryBuffer -> IO String
getMemoryBuffer h = do c <- vioc_get (vrv h)
                       return (snd c)

instance Show MemoryBuffer where
    show _ = "<MemoryBuffer>"

instance HVIOGeneric MemoryBuffer where
    vClose = vioc_close . vrv
    vIsEOF h = do vTestOpen h
                  c <- vioc_get (vrv h)
                  return ((length (snd c)) == (fst c))
    vIsOpen = vioc_isopen . vrv

instance HVIOReader MemoryBuffer where
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

instance HVIOWriter MemoryBuffer where
    vPutStr h s = do (pos, buf) <- vioc_get (vrv h)
                     let (pre, post) = splitAt pos buf
                     let newbuf = pre ++ s ++ (drop (length buf) post)
                     vioc_set (vrv h) (pos + (length buf), newbuf)
    vPutChar h c = vPutStr h [c]

instance HVIOSeeker MemoryBuffer where
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

{- | Create a Haskell pipe.

These pipes are analogous to the Unix
pipes that are available from System.Posix, but don't require Unix and work
only in Haskell.  When you create a pipe, you actually get two HVIO objects:
a 'PipeReader' and a 'PipeWriter'.  You must use the 'PipeWriter' in one
thread and the 'PipeReader' in another thread.  Data that's written to the
'PipeWriter' will then be available for reading with the 'PipeReader'.  The
pipes are implemented completely with existing Haskell threading primitives,
and require no special operating system support.  Unlike Unix pipes, these
pipes cannot be used across a fork().  Also unlike Unix pipes, these pipes
are portable and interact well with Haskell threads.
-}

newHVIOPipe :: IO (PipeReader, PipeWriter)
newHVIOPipe = do mv <- newEmptyMVar
                 readerref <- newIORef (True, mv)
                 let reader = PipeReader readerref
                 writerref <- newIORef (True, reader)
                 return (reader, PipeWriter writerref)

data PipeBit = PipeBit Char 
             | PipeEOF
               deriving (Eq, Show)

{- | The reading side of a Haskell pipe.  Please see 'newHVIOPipe' for more
details. -}
newtype PipeReader = PipeReader (VIOCloseSupport (MVar PipeBit))

{- | The writing side of a Haskell pipe.  Please see 'newHVIOPipe' for more
details. -}
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
