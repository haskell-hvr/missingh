{- arch-tag: Generic Server Support
Copyright (c) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : Network.SocketServer
   Copyright  : Copyright (C) 2004-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : experimental
   Portability: systems with networking

This module provides an infrastructure to simplify server design.

Written by John Goerzen, jgoerzen\@complete.org

Please note: this module is designed to work with TCP, UDP, and Unix domain
sockets, but only TCP sockets have been tested to date.

This module is presently under-documented.  For an example of usage, please
see the description of "Network.FTP.Server".
-}

module Network.SocketServer(-- * Generic Options and Types
                                     InetServerOptions(..),
                                     simpleTCPOptions,
                                     SocketServer(..),
                                     HandlerT,
                                     -- * TCP server convenient setup
                                     serveTCPforever,
                                     -- * Lower-Level Processing
                                     setupSocketServer,
                                     handleOne,
                                     serveForever,
                                     closeSocketServer,
                                     -- * Combinators
                                     loggingHandler,
                                     threadedHandler,
                                     handleHandler
                                    )
where
import Network.Socket
import Network.BSD
import Network.Utils
import Control.Concurrent
import System.IO
import qualified System.Log.Logger

{- | Options for your server. -}
data InetServerOptions  = InetServerOptions {listenQueueSize :: Int,
                                             portNumber :: PortNumber,
                                             interface :: HostAddress,
                                             reuse :: Bool,
                                             family :: Family,
                                             sockType :: SocketType,
                                             protoStr :: String
                                            }
    deriving (Eq, Show)

{- | The main handler type.

The first parameter is the socket itself.

The second is the address of the remote endpoint.

The third is the address of the local endpoint.
-}
type HandlerT = Socket -> SockAddr -> SockAddr -> IO ()
                     
{- | Get Default options.  You can always modify it later. -}
simpleTCPOptions :: Int                -- ^ Port Number
                 -> InetServerOptions
simpleTCPOptions p = InetServerOptions {listenQueueSize = 5,
                                        portNumber = (fromIntegral p),
                                        interface = iNADDR_ANY,
                                        reuse = False,
                                        family = AF_INET,
                                        sockType = Stream,
                                        protoStr = "tcp"
                                       }

data SocketServer = SocketServer {optionsSS :: InetServerOptions,
                                  sockSS :: Socket}
                  deriving (Eq, Show)

{- | Takes some options and sets up the 'SocketServer'.  I will bind
and begin listening, but will not accept any connections itself. -}
setupSocketServer :: InetServerOptions -> IO SocketServer
setupSocketServer opts =
    do proto <- getProtocolNumber (protoStr opts)
       s <- socket (family opts) (sockType opts) proto
       setSocketOption s ReuseAddr (case (reuse opts) of
                                    True -> 1
                                    False -> 0)
       bindSocket s (SockAddrInet (portNumber opts)
                     (interface opts))
       listen s (listenQueueSize opts)
       return $ SocketServer {optionsSS = opts, sockSS = s}

{- | Close the socket server.  Does not terminate active
handlers, if any. -}
closeSocketServer :: SocketServer -> IO ()
closeSocketServer ss =
    sClose (sockSS ss)
       
{- | Handle one incoming request from the given 'SocketServer'. -}
handleOne :: SocketServer -> HandlerT -> IO ()
handleOne ss func =
    let opts = (optionsSS ss)
        in    do a <- accept (sockSS ss)
                 localaddr <- getSocketName (fst a)
                 func (fst a) (snd a) localaddr
    
{- | Handle all incoming requests from the given 'SocketServer'. -}
serveForever :: SocketServer -> HandlerT -> IO ()
serveForever ss func =
    sequence_ (repeat (handleOne ss func))

{- | Convenience function to completely set up a TCP
'SocketServer' and handle all incoming requests.

This function is literally this:

>serveTCPforever options func =
>    do sockserv <- setupSocketServer options
>       serveForever sockserv func
 -}
serveTCPforever :: InetServerOptions     -- ^ Server options
                -> HandlerT              -- ^ Handler function
                -> IO ()                
serveTCPforever options func =
    do sockserv <- setupSocketServer options
       serveForever sockserv func

----------------------------------------------------------------------
-- Combinators
----------------------------------------------------------------------

{- | Log each incoming connection using the interface in
"System.Log.Logger".

Log when the incoming connection disconnects.

Also, log any failures that may occur in the child handler. -}

loggingHandler :: String                -- ^ Name of logger to use
               -> System.Log.Logger.Priority -- ^ Priority of logged messages
               -> HandlerT              -- ^ Handler to call after logging
               -> HandlerT              -- ^ Resulting handler
loggingHandler hname prio nexth socket r_sockaddr l_sockaddr = 
    do sockStr <- showSockAddr r_sockaddr
       System.Log.Logger.logM hname prio 
                   ("Received connection from " ++ sockStr)
       System.Log.Logger.traplogging hname 
               System.Log.Logger.WARNING "" (nexth socket r_sockaddr 
                                                   l_sockaddr)
       System.Log.Logger.logM hname prio
                   ("Connection " ++ sockStr ++ " disconnected")
       

-- | Handle each incoming connection in its own thread to
-- make the server multi-tasking.
threadedHandler :: HandlerT             -- ^ Handler to call in the new thread
                -> HandlerT             -- ^ Resulting handler
threadedHandler nexth socket r_sockaddr l_sockaddr=
    do forkIO (nexth socket r_sockaddr l_sockaddr)
       return ()

{- | Give your handler function a Handle instead of a Socket.

The Handle will be opened with ReadWriteMode (you use one handle for both
directions of the Socket).  Also, it will be initialized with LineBuffering.

Unlike other handlers, the handle will be closed when the function returns.
Therefore, if you are doing threading, you should to it before you call this
handler.
-}
handleHandler :: (Handle -> SockAddr -> SockAddr -> IO ())      -- ^ Handler to call
              -> HandlerT
handleHandler func socket r_sockaddr l_sockaddr = 
    do h <- socketToHandle socket ReadWriteMode
       hSetBuffering h LineBuffering
       func h r_sockaddr l_sockaddr
       hClose h
