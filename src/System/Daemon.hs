{-# LANGUAGE CPP #-}
{-
Copyright (c) 2005-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : System.Daemon
   Copyright  : Copyright (C) 2005-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable to platforms with POSIX process\/signal tools

Tools for writing daemons\/server processes

Written by John Goerzen, jgoerzen\@complete.org

Please note: Most of this module is not compatible with Hugs.

Messages from this module are logged under @System.Daemon@.  See
'System.Log.Logger' for details.

Based on background
from <http://www.erlenstar.demon.co.uk/unix/faq_2.html#SEC16> and
<http://www.haskell.org/hawiki/HaskellUnixDaemon>.

This module is not available on Windows.
-}

module System.Daemon (

#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
        detachDaemon
#endif
                   )
                       where
#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))

import System.Posix.Process
import System.Posix.IO
import System.Directory
import System.Log.Logger
import System.Exit


trap :: IO a -> IO a
trap = traplogging "System.Daemon" ERROR "detachDaemon"

{- | Detach the process from a controlling terminal and run it in the
background, handling it with standard Unix deamon semantics.

After running this, please note the following side-effects:

 * The PID of the running process will change

 * stdin, stdout, and stderr will not work (they'll be set to
   \/dev\/null)

 * CWD will be changed to \/

I /highly/ suggest running this function before starting any threads.

Note that this is not intended for a daemon invoked from inetd(1).
-}
detachDaemon :: IO ()
detachDaemon = trap $
               do forkProcess child1
                  exitImmediately ExitSuccess

child1 :: IO ()
child1 = trap $
    do createSession
       forkProcess child2
       exitImmediately ExitSuccess

child2 :: IO ()
child2 = trap $
    do setCurrentDirectory "/"
       mapM_ closeFd [stdInput, stdOutput, stdError]
       nullFd <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
       mapM_ (dupTo nullFd) [stdInput, stdOutput, stdError]
       closeFd nullFd
#endif
