{- arch-tag: Network utilities main file
Copyright (C) 2004-2005 John Goerzen <jgoerzen@complete.org>

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
   Module     : MissingH.Network
   Copyright  : Copyright (C) 2004-2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: systems with networking

This module provides various helpful utilities for dealing with networking

Written by John Goerzen, jgoerzen\@complete.org

-}

module MissingH.Network(niceSocketsDo, connectTCP, connectTCPAddr,
                        listenTCPAddr, showSockAddr
                       )
where
import Network
import Network.Socket
import Network.BSD
import System.IO
import qualified System.Posix.Signals

{- | Sets up the system for networking.  Similar to the built-in
withSocketsDo (and actually, calls it), but also sets the SIGPIPE
handler so that signal is ignored. 

Example:

> main = niceSocketsDo $ do { ... } 
-}

-- FIXME integrate with WebCont.Util.UDP

niceSocketsDo :: IO a -> IO a
niceSocketsDo func = do
#ifndef mingw32_HOST_OS
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
#ifndef mingw32_HOST_OS
showSockAddr (SockAddrUnix x) = return $ "UNIX socket at " ++ x
#endif
showSockAddr (SockAddrInet port host) =
    do h <- inet_ntoa host
       return $ "IPv4 host " ++ h ++ ", port " ++ (show port)
