{- arch-tag: Network utilities main file
Copyright (c) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}
{- |
   Module     : Network.Utils
   Copyright  : Copyright (C) 2004-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: systems with networking

This module provides various helpful utilities for dealing with networking

Written by John Goerzen, jgoerzen\@complete.org

-}

module Network.Utils (niceSocketsDo, connectTCP, connectTCPAddr,
                        listenTCPAddr, showSockAddr)
    where

import Network
import Network.Socket
import Network.BSD
import System.IO
#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
import qualified System.Posix.Signals
#endif

{- | Sets up the system for networking.  Similar to the built-in
withSocketsDo (and actually, calls it), but also sets the SIGPIPE
handler so that signal is ignored.

Example:

> main = niceSocketsDo $ do { ... }
-}

-- FIXME integrate with WebCont.Util.UDP

niceSocketsDo :: IO a -> IO a
niceSocketsDo func = do
#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
                -- No signals on Windows anyway
                System.Posix.Signals.installHandler
                      System.Posix.Signals.sigPIPE
                      System.Posix.Signals.Ignore
                      Nothing
#endif
                withSocketsDo func

connectTCP :: HostName -> PortNumber -> IO Socket
connectTCP host port = do
                       he <- getHostByName host
                       connectTCPAddr (SockAddrInet port (hostAddress he))

connectTCPAddr :: SockAddr -> IO Socket
connectTCPAddr addr = do
                      proto <- getProtocolNumber "tcp"
                      s <- socket AF_INET Stream proto
                      connect s addr
                      return s

listenTCPAddr :: SockAddr -> Int -> IO Socket
listenTCPAddr addr queuelen = do
                     proto <- getProtocolNumber "tcp"
                     s <- socket AF_INET Stream proto
                     bindSocket s addr
                     listen s queuelen
                     return s

showSockAddr :: SockAddr -> IO String
#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
showSockAddr (SockAddrUnix x) = return $ "UNIX socket at " ++ x
#endif
showSockAddr (SockAddrInet port host) =
    do h <- inet_ntoa host
       return $ "IPv4 host " ++ h ++ ", port " ++ (show port)
