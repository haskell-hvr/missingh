{- |
   Module      :  Control.Concurrent.Child
   Copyright   :  (c) 2005-02-10 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre

   "Control.Concurrent"'s 'forkIO' is as low-level as it
   gets: once you have forked an 'IO' thread, it is
   basically /gone/; all @forkIO@ returns is @()@. If you
   need more sophisticated control, you have to do it
   yourself. This is what this library does.
-}

module Control.Concurrent.Child
  ( Child(..)   --  = Child ThreadId (MVar a)
  , mkChild     --  :: Monoid a => ThreadId -> MVar a -> IO a -> IO ()
  , spawn       --  :: Monoid a => IO a -> IO (Child a)
  , wait        --  :: Child a -> IO a
  , send        --  :: Child a -> Exception -> IO ()
  , kill        --  :: Child a -> IO ()
  , par         --  :: Monoid a => IO a -> IO a -> IO a
  )
  where

import Prelude hiding ( catch )
import Control.Exception
import Control.Concurrent
import Control.Monad
import Data.Monoid

-- |A @Child@ is a cutely named data type which contains the
-- 'ThreadId' of a forked computation and an 'MVar' into
-- which the computation will put its return value once it
-- has terminated. Thus, you can wait for your children to
-- terminate by getting the value from that 'MVar'. Another
-- property of children is that uncaught exceptions in them
-- will be propagated to your thread. So either you 'catch'
-- them, or a failure in any however deeply nested child
-- will go right up to @main@.

data Child a = Child ThreadId (MVar a)

-- |Wrap an 'IO' computation so that (1) uncaught exceptions
-- are re-thrown to the given 'ThreadId', and that (2) it
-- will put the @a@ it returns into the 'MVar'. The value
-- has to be a 'Monoid' because in case of (1) the wrapper
-- still needs /some/ value to put into that 'MVar', so it
-- uses 'mempty'. Popular monoids are @()@ and @[]@.

mkChild :: (Monoid a) => ThreadId -> MVar a -> IO a -> IO ()
mkChild parent mv f = catch (f >>= sync) h `finally` sync mempty
  where
  sync x = tryPutMVar mv x >> return ()
  h (AsyncException ThreadKilled) = return ()
  h e                             = throwTo parent e

-- |Start an 'IO' computation with the properties described
-- above.

spawn :: (Monoid a) => IO a -> IO (Child a)
spawn f = do
  self <- myThreadId
  sync <- newEmptyMVar
  pid  <- forkIO (mkChild self sync f)
  return (Child pid sync)

-- |Get the value returned by a \"child process\"; may be
-- 'mempty'. But in case it is, you have just received an
-- asynchronous 'Exception' anyway, so you have other things
-- to do. The function does not return until the child has
-- terminated. If /your/ thread receives an exception while
-- it waits for the child, the child will be terminated
-- before 'wait' returns. So once 'wait' returns, the child
-- is guaranteed to be gone, one way or another.

wait :: Child a -> IO a
wait (Child pid sync) = readMVar sync `finally` killThread pid

-- |A fancy wrapper for 'throwTo'.

send :: Child a -> Exception -> IO ()
send (Child pid _) = throwTo pid

-- |Wraps 'killThread'.

kill :: Child a -> IO ()
kill (Child pid _) = killThread pid

-- |Run both computations with 'spawn' and return the value
-- of the child which terminates /first/. Both children are
-- guaranteed to be gone when 'par' returns. Exceptions in
-- either child are propagated. So if either child fails,
-- 'par' fails.

par :: (Monoid a) => IO a -> IO a -> IO a
par f g = do
  self <- myThreadId
  sync <- newEmptyMVar
  bracket
    (forkIO (mkChild self sync f))
    (killThread)
    (\_ -> bracket
             (forkIO (mkChild self sync g))
             (killThread)
             (\_ -> takeMVar sync))
