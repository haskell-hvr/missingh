{-# OPTIONS -fglasgow-exts #-}
{- |
   Module      :  System.IO.BlockIO
   Copyright   :  (c) 2005-02-10 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre

   'runLoop' drives a 'BlockHandler' with data read from the
   input stream until 'hIsEOF' ensues. Everything else has
   to be done by the callback; runLoop just does the I\/O.
   But it does it /fast/.
-}

module System.IO.BlockIO where

import Prelude hiding ( catch, rem )
import Control.Exception
import Control.Monad.State
import Data.List
import Data.Typeable
import System.IO
import System.IO.Error hiding ( catch )
import Foreign  hiding ( new )
import MissingH.Threads.Timeout

-- * Static Buffer I\/O

type ReadHandle  = Handle
type WriteHandle = Handle

type ByteCount = Word16
type Capacity  = Word16
data Buffer    = Buf !Capacity !(Ptr Word8) !ByteCount
                 deriving (Eq, Show, Typeable)

-- |Run the given computation with an initialized, empty
-- 'Buffer'. The buffer is gone when the computation
-- returns.

withBuffer :: Capacity -> (Buffer -> IO a) -> IO a
withBuffer 0 = fail "BlockIO.withBuffer with size 0 doesn't make sense"
withBuffer n = bracket cons dest
  where
  cons = mallocArray (fromIntegral n) >>= \p -> return (Buf n p 0)
  dest = \(Buf _ p _) -> free p

-- |Drop the first @n <= size@ octets from the buffer.

flush :: ByteCount -> Buffer -> IO Buffer
flush 0 buf               = return buf
flush n (Buf cap ptr len) = assert (n <= len) $ do
  let ptr' = ptr `plusPtr` (fromIntegral n)
      len' = (fromIntegral len) - (fromIntegral n)
  when (len' > 0) (copyArray ptr ptr' len')
  return (Buf cap ptr (fromIntegral len'))

-- |If there is space, read and append more octets; then
-- return the modified buffer. In case of 'hIsEOF',
-- 'Nothing' is returned. If the buffer is full already,
-- 'throwDyn' a 'BufferOverflow' exception. When the timeout
-- exceeds, 'ReadTimeout' is thrown.

slurp :: Timeout -> ReadHandle -> Buffer -> IO (Maybe Buffer)
slurp to h b@(Buf cap ptr len) = do
  when (cap <= len) (throwDyn (BufferOverflow h b))
  timeout to (handleEOF wrap) >>=
    maybe (throwDyn (ReadTimeout to h b)) return
  where
  wrap = do let ptr' = ptr `plusPtr` (fromIntegral len)
                n    = cap - len
            rc <- hGetBufNonBlocking h ptr' (fromIntegral n)
            if rc > 0
               then return (Buf cap ptr (len + (fromIntegral rc)))
               else hWaitForInput h (-1) >> wrap

-- * BlockHandler and I\/O Driver

-- |A callback function suitable for use with 'runLoop'
-- takes a buffer and a state, then returns a modified
-- buffer and a modified state. Usually the callback will
-- use 'slurp' to remove data it has processed already.

type BlockHandler st = Buffer -> st -> IO (Buffer, st)

-- |Our main I\/O driver.

runLoopNB
  :: (st -> Timeout)            -- ^ user state provides timeout
  -> (Exception -> st -> IO st) -- ^ user provides I\/O error handler
  -> ReadHandle                 -- ^ the input source
  -> Capacity                   -- ^ I\/O buffer size
  -> BlockHandler st            -- ^ callback
  -> st                         -- ^ initial callback state
  -> IO st                      -- ^ return final callback state
runLoopNB mkTO errH hIn cap f initST = withBuffer cap (flip ioloop $ initST)
  where
  ioloop buf st = buf `seq` st `seq`
    handle (\e -> errH e st) $ do
      rc <- slurp (mkTO st) hIn buf
      case rc of
        Nothing   -> return st
        Just buf' -> f buf' st >>= uncurry ioloop

-- |A variant which won't time out and will just 'throw' all
-- exceptions.

runLoop :: ReadHandle -> Capacity -> BlockHandler st -> st -> IO st
runLoop = runLoopNB (const (-1)) (\e _ -> throw e)

-- * Handler Combinators

-- |Signal how many bytes have been consumed from the
-- /front/ of the list; these octets will be dropped.

type StreamHandler st = [Word8] -> st -> IO (ByteCount, st)

handleStream :: StreamHandler st -> BlockHandler st
handleStream f buf@(Buf _ ptr len) st = do
  (i, st') <- peekArray (fromIntegral len) ptr >>= (flip f) st
  buf' <- flush i buf
  return (buf', st')

-- * I\/O Exceptions

-- |Thrown by 'slurp'.

data BufferOverflow = BufferOverflow ReadHandle Buffer
                    deriving (Show, Typeable)

-- |Thrown by 'slurp'.

data ReadTimeout    = ReadTimeout Timeout ReadHandle Buffer
                    deriving (Show, Typeable)


-- * Internal Helper Functions

-- |Return 'Nothing' if the given computation throws an
-- 'isEOFError' exception. Used by 'slurp'.

handleEOF :: IO a -> IO (Maybe a)
handleEOF f =
  catchJust ioErrors
    (fmap Just f)
    (\e -> if isEOFError e then return Nothing else ioError e)

-- |Our version of C's @strstr(3)@.

strstr :: [Word8] -> [Word8] -> Maybe Int
strstr tok = strstr' 0
  where
  strstr'  _     []       = Nothing
  strstr' pos ls@(_:xs)
    | tok `isPrefixOf` ls = Just (pos + length tok)
    | otherwise           = strstr' (pos + 1) xs

-- |Split a list by some delimiter. Will soon be provided by
-- "Data.List".

splitList :: Eq a => [a] -> [a] -> [[a]]
splitList d' l' =
  unfoldr (\x -> (null x) ? (Nothing, Just $ nextToken d' [] (snd $ splitAt (length d') x))) (d'++l')
  where nextToken _ r [] = (r, [])
        nextToken d r l@(h:t) | (d `isPrefixOf` l) = (r, l)
                              | otherwise = nextToken d (r++[h]) t

-- |Shorthand for if-then-else. Will soon by provided by
-- "Data.Bool".

(?) :: Bool -> (a,a) -> a
(?) True  (x,_) = x
(?) False (_,x) = x
