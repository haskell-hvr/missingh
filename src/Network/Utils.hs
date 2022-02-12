{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}

{- arch-tag: Network utilities main file
Copyright (c) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}
{- |
   Module     : Network.Utils
   Copyright  : Copyright (C) 2004-2011 John Goerzen
   SPDX-License-Identifier: BSD-3-Clause

   Stability  : stable
   Portability: systems with networking

This module provides various helpful utilities for dealing with networking

Written by John Goerzen, jgoerzen\@complete.org

-}

module Network.Utils (niceSocketsDo, connectTCP, connectTCPAddr,
                        listenTCPAddr, showSockAddr)
    where

import           Network.BSD
import           Network.Socket
import           System.IO
#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
import qualified System.Posix.Signals
#endif
import           Control.Exception (bracketOnError)

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
                      bracketOnError (socket AF_INET Stream proto) close
                        (\s -> connect s addr >> return s)

listenTCPAddr :: SockAddr -> Int -> IO Socket
listenTCPAddr addr queuelen = do
                     proto <- getProtocolNumber "tcp"
                     s <- socket AF_INET Stream proto
                     bind s addr
                     listen s queuelen
                     return s

showSockAddr :: SockAddr -> IO String
#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
showSockAddr (SockAddrUnix x) = return $ "UNIX socket at " ++ x
#endif
showSockAddr sa@(SockAddrInet port host) =
    do (Just h,_) <- getNameInfo [NI_NUMERICHOST] True False sa
       return $ "IPv4 host " ++ h ++ ", port " ++ (show port)
