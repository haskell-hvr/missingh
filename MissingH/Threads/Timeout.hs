{- |
   Module      :  MissingH.Threads.Timeout
   Copyright   :  (c) 2005-02-10 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre

   Timeout support for 'IO' computations.
-}

module MissingH.Threads.Timeout
  ( Timeout     --  = Int
  , timeout     --  :: Timeout -> IO a -> IO (Maybe a)
  )
  where

import MissingH.Threads.Child ( par )
import Control.Concurrent ( threadDelay )

-- |Timeouts are given in microseconds (@1\/10^6@ seconds).
-- Negative values generally mean \"wait indefinitely\".
-- Make sure you don't exceed @maxBound :: Int@ when
-- specifying large timeouts!

type Timeout = Int

-- |Wrap an 'IO' computation to timeout and return 'Nothing'
-- after @n@ microseconds, otherwise @'Just' a@ is returned.

timeout :: Timeout -> IO a -> IO (Maybe a)
timeout n f
  | n < 0     = fmap Just f
  | n == 0    = return Nothing
  | otherwise = do                       --     a => [a]
      r <- par (threadDelay n >> return []) (fmap return f)
      case r of []    -> return Nothing
                (a:_) -> return (Just a)
                        -- We do this so that @a@ doesn't have
                        -- to be a monoid.
