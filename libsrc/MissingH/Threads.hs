{- arch-tag: Thread utilities main file
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
   Module     : MissingH.Threads
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

This module provides various helpful utilities for dealing with threads.

Written by John Goerzen, jgoerzen\@complete.org
-}

module MissingH.Threads(-- * I\/O utilities
                        runInThread
                       )
where

import Control.Concurrent

{- | Takes a IO action and a function.  The IO action will be called in a 
separate thread.  When it is completed, the specified function is called with
its result.  This is a simple way of doing callbacks. -}

runInThread :: IO a -> (a -> IO b) -> IO ThreadId
runInThread action callback = forkIO $ action >>= callback >> return ()
