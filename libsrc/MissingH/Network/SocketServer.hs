{- arch-tag: Generic Server Support
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
   Module     : MissingH.Network.SocketServer
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : experimental
   Portability: systems with networking

This module provides an infrastructure to simplify server design.

Written by John Goerzen, jgoerzen\@complete.org
-}

module MissingH.Network.SocketServer(-- * Generic Options and Types
                                     InetServerOptions(..),
                                     simpleInetOptions,
                                     HandlerT,
                                     -- * TCP server handlers
                                     serveTCPforever,
                                     -- * Combinators
                                     loggingHandler,
                                     threadedHandler,
                                     handleHandler
                                    )
where
import Network.Socket
import Network.BSD
import MissingH.Network
import Control.Concurrent
import System.IO
import qualified MissingH.Logging.Logger

{- | Options for your server. -}
data InetServerOptions  = InetServerOptions {listenQueueSize :: Int,
                                             portNumber :: Int,
                                             interface :: HostAddress,
                                             reuse :: Bool,
                                             family :: Family
                                            }
    deriving (Eq, Show)

type HandlerT = (Socket -> SockAddr -> IO ())
                     
{- | Get Default options.  You can always modify it later. -}
simpleInetOptions :: Int                -- ^ Port Number
                 -> InetServerOptions
simpleInetOptions p = InetServerOptions {listenQueueSize = 5,
                                        portNumber = p,
                                        interface = iNADDR_ANY,
                                        reuse = False,
                                        family = AF_INET
                                       }

serveTCPforever :: InetServerOptions     -- ^ Server options
                -> HandlerT              -- ^ Handler function
                -> IO ()                
serveTCPforever options func =
    do proto <- getProtocolNumber "tcp"
       s <- socket (family options) Stream proto
       bindSocket s (SockAddrInet (fromIntegral (portNumber options)) 
                     (interface options))
       listen s (listenQueueSize options)
       let run = do a <- accept s
                    func (fst a) (snd a)
       sequence_ (repeat run)

----------------------------------------------------------------------
-- Combinators
----------------------------------------------------------------------

-- | Log each incoming connection using the interface in
-- "MissingH.Logging.Logger".

loggingHandler :: String                -- ^ Name of logger to use
               -> MissingH.Logging.Logger.Priority -- ^ Priority of logged messages
               -> HandlerT              -- ^ Handler to call after logging
               -> HandlerT              -- ^ Resulting handler
loggingHandler hname prio nexth socket sockaddr = 
    do sockStr <- showSockAddr sockaddr
       MissingH.Logging.Logger.logM hname prio 
                   ("Received connection from " ++ sockStr)
       nexth socket sockaddr

-- | Handle each incoming connection in its own thread to
-- make the server multi-tasking.
threadedHandler :: HandlerT             -- ^ Handler to call in the new thread
                -> HandlerT             -- ^ Resulting handler
threadedHandler nexth socket sockaddr =
    do forkIO (nexth socket sockaddr)
       return ()

-- | Give your handler function a Handle instead of a Socket and SockAddr.
handleHandler :: (Handle -> IO ())      -- ^ Handler to call
              -> HandlerT
handleHandler func socket _ = 
    socketToHandle socket ReadWriteMode >>= func
