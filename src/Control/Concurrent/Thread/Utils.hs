{- arch-tag: Thread utilities main file
Copyright (c) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : Control.Concurrent.Thread.Utils
   Copyright  : Copyright (C) 2004-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable

This module provides various helpful utilities for dealing with threads.

Written by John Goerzen, jgoerzen\@complete.org
-}

module Control.Concurrent.Thread.Utils(-- * I\/O utilities
                        runInThread
                       )
where

import Control.Concurrent

{- | Takes a IO action and a function.  The IO action will be called in a 
separate thread.  When it is completed, the specified function is called with
its result.  This is a simple way of doing callbacks. -}

runInThread :: IO a -> (a -> IO b) -> IO ThreadId
runInThread action callback = forkIO $ action >>= callback >> return ()
