{- |
   Module      :  BlockIO
   Copyright   :  (c) 2004-11-12 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre

   All you need from this module is the 'runLoop' function
   and the 'Handler' type definition. It's no
   breath-takingly inventive concept, actually: @runLoop@
   will read blocks of 'Word8' from a 'Handle' into a static
   buffer on the heap, and every time new input has arrived,
   it will call a handler with a 'Ptr' into the buffer, so
   that the handler can do stuff with the data. Then, the
   handler returns either: I need more data before I can do
   anything ('Nothing'), or I have consumed @n@ bytes,
   please flush them (@'Just' (a,Int)@).

   Basically, 'runLoop' drives the handler with the data
   read from the input stream until 'hIsEOF' ensues.
   Everything else has to be done by the handler -- the
   'BlockIO' monad just does the I\/O. But it does it
   /fast/.
-}

module MissingH.IO.BlockIO where

import Prelude hiding ( rem )
import System.IO
import System.IO.Error
import Foreign  hiding ( new )
import Control.Monad.State
import Control.Exception   ( assert, bracket )
import MissingH.Threads.Child               ( Timeout, timeout )

-- * I\/O Buffer

-- |Capacity, pointer into the heap, and length of contents;
-- all sizes are in bytes, naturally.

data Buffer = Buf !Int !(Ptr Word8) !Int
              deriving (Show)

-- |Initialize a 'Buffer' of capacity @n > 0@ and run the
-- given 'IO' computation with it. The buffer will be
-- destroyed when the computation returns.

withBuffer :: Int -> (Buffer -> IO a) -> IO a
withBuffer n f = assert (n > 0) bracket (cons) (dest) f
  where
  cons = mallocArray n >>= \p -> return (Buf n p 0)
  dest = \(Buf _ p _) -> free p

-- |Drop the first @0 <= n <= size@ octets from the buffer.

flushBuffer :: Int -> Buffer -> IO Buffer
flushBuffer n (Buf cap ptr len) =
  assert (n >= 0) $
    assert (n <= len) $ do
       let ptr' = ptr `plusPtr` n
           len' = len - n
       when (len' > 0) (copyArray ptr ptr' len')
       return (Buf cap ptr len')

-- |If there is space in the 'Buffer', read and append more
-- octets, then return the modified buffer. In case of
-- @EOF@, 'Nothing' is returned. If the buffer is full
-- already, raise an 'IOError'. Use the 'isBufferOverflow'
-- predicate to test an exception you caught for this
-- condition.

readBuffer :: Handle -> Buffer -> IO (Maybe Buffer)
readBuffer h (Buf cap ptr len)
  | cap <= len = fail "BlockIO: buffer overflow"
  | otherwise  = handleEOF wrap
      where
      wrap = do let ptr' = (ptr `plusPtr` len)
                    n    = (cap - len)
                rc <- hGetBufNonBlocking h ptr' n
                if rc > 0
                   then return (Buf cap ptr (len + rc))
                   else hWaitForInput h (-1) >> wrap

-- * BlockIO Monad

-- |The internal state of a block-I\/O execution thread.

data BIOState st = BIO !Handle !Buffer !st
                   deriving (Show)

-- |A block-I\/O execution thread.

type BlockIO st a = StateT (BIOState st) IO a

-- |Run a 'BlockIO' computation in the 'IO' monad. The
-- contents of the I\/O buffer is lost when 'runBlockIO'
-- returns. If you need more control, use 'withBuffer' to
-- construct your own 'BIOState' and run the monad with
-- 'runStateT'.

runBlockIO :: Handle
	   -> Int               -- ^ buffer capacity
	   -> BlockIO st a      -- ^ computation to run
	   -> st                -- ^ initial state
	   -> IO (a, st)
runBlockIO h size f st =
  withBuffer size $ \buf -> do
    (a, BIO _ _ st') <- runStateT f (BIO h buf st)
    return (a,st')

-- ** Primitives

-- |Read some more input data into the I\/O buffer. Returns
-- 'True' if there is more, 'False' when @EOF@ occurs. May
-- raise an 'isBufferOverflow' or 'isTimeout' exception.

slurp :: Timeout -> BlockIO st Bool
slurp to = do
  BIO h b st <- get
  r <- liftIO $ timeout to (readBuffer h b)
                  >>= maybe (fail "BlockIO: read timeout") return
  case r of
    Nothing -> return False
    Just b' -> put (BIO h b' st) >> return True

-- |Drop the first @0 <= n <= size@ octets from the I\/O
-- buffer.

flush :: Int -> BlockIO st ()
flush 0 = return ()
flush n = do
  BIO h b st <- get
  b' <- liftIO (flushBuffer n b)
  put (BIO h b' st)

-- ** Handler and I\/O Driver

-- |A handler is a stateful 'IO' computation which the I\/O
-- driver 'consume' calls every time new input has arrived.
-- If the buffer contains enough data to do something useful
-- with it, it should do it, and signal how many octets have
-- been consumed with the returned @Int@. These will be
-- flushed from the beginning of buffer when the handler is
-- called the next time. The handler itself should /not/
-- modify the buffer.

type Handler st a = (Ptr Word8, Int)
                  -> StateT st IO (Maybe (a,Int))

-- |A handler which can be run in an I\/O loop without ever
-- needing to return values to the main program, for
-- instance with 'runLoop'.

type LoopHandler st = Handler st ()

-- |Use the given handler to consume a token from the I\/O
-- buffer. Returns 'Nothing' in case of @EOF@. May raise an
-- 'isBufferOverflow' or 'isTimeout' exception.

consume :: Timeout -> Handler st a -> BlockIO st (Maybe a)
consume to f = do
  more <- slurp to
  BIO h b@(Buf _ ptr len) st <- get
  (tok, st') <- liftIO (runStateT (f (ptr,len)) st)
  put (BIO h b st')
  case (tok,more) of
    (Just (a,n), _)  -> flush n >> return (Just a)
    (Nothing, True)  -> consume to f
    (Nothing, False) -> return Nothing

-- |Repeated 'consume' until @EOF@. The handler may only
-- return @()@ to prevent space leaks. Use @st@ instead.
-- @:-)@

loop :: (st -> Timeout) -> LoopHandler st -> BlockIO st ()
loop to f = get >>= \(BIO _ _ st) ->
  consume (to st) f >>= maybe (return ()) (const (loop to f))

-- |Iterate a handle until @EOF@ and use the given 'Handler'
-- to do stuff with the data we've read. The @(st ->
-- Timeout)@ function will be used to to determine the read
-- timeout from the current handler state.

runLoopNB :: (st -> Timeout)    -- ^ user state provides timeout
          -> Handle             -- ^ read from here
          -> Int                -- ^ buffer size to allocate
          -> LoopHandler st     -- ^ call-back to handle the data
          -> st                 -- ^ initial handler state
          -> IO st              -- ^ the state at time of @EOF@
runLoopNB to h size f st =
  fmap snd (runBlockIO h size (loop to f) st)

-- | @runLoop@ @=@ @'runLoopNB' (const (-1))@

runLoop :: Handle -> Int -> LoopHandler st -> st -> IO st
runLoop = runLoopNB (const (-1))

-- * Handler Combinators

-- ** Lines

-- |Wrap an integer signifying the front gap of the buffer
-- into the user state. Initialize it to @0@.

type LineHandler st a = Handler (Int,st) [a]

-- |Split the input buffer into lines using \'@\\n@\' as
-- end-of-line marker. The \'@\\n@\' is /not/ stripped. Then
-- call the given 'IO' computation line-by-line until all
-- lines have been consumed.

handleLines :: (String -> StateT st IO a) -> LineHandler st a
handleLines f (ptr,len) = do
  (gap,st) <- get
  when (len < gap) (fail "BlockIO.handleLines: inconsistent buffer")
  if len == gap
     then return Nothing
     else do
       buf <- liftIO (peekArray len ptr)
       let buf'      = map (toEnum . fromEnum) buf
           (old,new) = splitAt gap buf'
           (ls,rem)  = splitBy (=='\n') new
           gap'      = length rem
       case ls of
         []     -> do put (len,st) >> return Nothing
         (x:xs) -> do
           (as,st') <- liftIO (runStateT (mapM f ((old++x):xs)) st)
           put (gap',st')
           return (Just (as,len-gap'))

-- * Error Handling

-- |Predicate to determine whether a caught 'IOError' was
-- caused by a buffer overflow.

isBufferOverflow :: IOError -> Bool
isBufferOverflow e =
  (isUserError e) && (ioeGetErrorString e == "BlockIO: buffer overflow")

-- |Determine whether a given exception is a read timeout
-- error raised by the BlockIO code.

isTimeout :: IOError -> Bool
isTimeout e =
  (isUserError e) && (ioeGetErrorString e == "BlockIO: read timeout")

-- * Internal Helper Functions

-- |Return 'Nothing' if the given computation throws an
-- 'isEOFError' exception. Used by 'readBuffer'.

handleEOF :: IO a -> IO (Maybe a)
handleEOF f =
  catch (f >>= return . Just)
    (\e -> case isEOFError e of True  -> return Nothing
                                False -> ioError e)

splitBy :: (a -> Bool) -> [a] -> ([[a]], [a])
splitBy f as = foldr coll ([],[]) (splitBy' [] as)
  where
  coll (Right x) (ls, rem) = (x : ls, rem)
  coll (Left x)  (ls, rem) = (ls, rem ++ x)

  splitBy'  [] []     = []
  splitBy' acc []     = Left acc : []
  splitBy' acc (x:xs) =
    if f x
       then Right (acc++[x]) : splitBy' [] xs
       else splitBy' (acc++[x]) xs
