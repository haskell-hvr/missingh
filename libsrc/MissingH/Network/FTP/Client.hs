{- arch-tag: FTP client support
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
   Module     : MissingH.Network.FTP.Client
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: systems with networking

This module provides a client-side interface to the File Transfer Protocol.

Written by John Goerzen, jgoerzen\@complete.org

Useful standards:

* RFC959, <http://www.cse.ohio-state.edu/cgi-bin/rfc/rfc0959.html>

* Passive mode, RFC1579, <http://www.cse.ohio-state.edu/cgi-bin/rfc/rfc1579.html>

* Extended passive mode, IPv6, RFC2428 <http://www.cse.ohio-state.edu/cgi-bin/rfc/rfc2428.html>

* Feature negotiation, RFC2389, <http://www.cse.ohio-state.edu/cgi-bin/rfc/rfc2389.html>

* Internationalization of FTP, RFC2640, <http://www.cse.ohio-state.edu/cgi-bin/rfc/rfc2640.html>

* FTP security considerations, RFC2577, <http://www.cse.ohio-state.edu/cgi-bin/rfc/rfc2577.html>

* FTP URLs, RFC1738, <http://www.cse.ohio-state.edu/cgi-bin/rfc/rfc1738.html>

-}

module MissingH.Network.FTP.Client(easyConnectTo, connectTo,
                                   loginAnon, login, 
                                   setPassive,
                                   FTPConnection(isPassive),
                       )
where
import MissingH.Network.FTP.Parser
import Network.BSD
import Network.Socket
import qualified Network
import System.IO
import System.IO.Unsafe
import MissingH.Logging.Logger

data FTPConnection = FTPConnection {readh :: IO String,
                                    writeh :: Handle,
                                    isPassive :: Bool}
                   deriving Eq

{-
getresp h = do c <- hGetContents h
               return (parseGoodReply c)
-}

getresp h = do
            c <- (readh h)
            debugParseGoodReply c

unexpectedresp m r = "Expected " ++ m ++ ", got " ++ (show r)

isxresp desired (r, _) = r >= desired && r < (desired + 100)

forcexresp desired r = if isxresp desired r
                       then r
                       else error ((unexpectedresp (show desired)) r)

forceioresp :: Int -> FTPResult -> IO ()
forceioresp desired r = if isxresp desired r
                        then return ()
                        else fail (unexpectedresp (show desired) r)

logsend m = debugM "MissingH.Network.FTP.Client" ("FTP sent: " ++ m)
sendcmd h c = do logsend c
                 hPutStr (writeh h) (c ++ "\r\n")
                 getresp h

{- | Connect to the remote FTP server and read but discard
   the welcome.  Assumes
   default FTP port, 21, on remote. -}
easyConnectTo :: Network.HostName -> IO FTPConnection
easyConnectTo h = do x <- connectTo h 21
                     return (fst x)

{- | Connect to remote FTP server and read the welcome. -}
connectTo :: Network.HostName -> PortNumber -> IO (FTPConnection, FTPResult)
connectTo h p =
    let readchars :: Handle -> IO String
        readchars h = do
                      c <- hGetChar h
                      next <- unsafeInterleaveIO $ readchars h
                      return (c : next)
        in
    do
    updateGlobalLogger "MissingH.Network.FTP.Parser" (setLevel DEBUG)
    updateGlobalLogger "MissingH.Network.FTP.Client" (setLevel DEBUG)
    proto <- getProtocolNumber "tcp"
    he <- getHostByName h
    s <- socket AF_INET Stream proto
    connect s (SockAddrInet p (hostAddress he))
    r <- socketToHandle s ReadMode
    hSetBuffering r LineBuffering
    w <- socketToHandle s WriteMode
    hSetBuffering w LineBuffering
    let h = FTPConnection {readh = readchars r, writeh = w, isPassive = True}
    --hIsReadable h >>= print
    --hIsWritable h >>= print
    -- hSetBuffering h LineBuffering
    resp <- getresp h
    forceioresp 200 resp
    --foo <- return (forcexresp 200 resp)
    --print foo
    -- hPutStr h "foo"
    -- resp `seq` return (h, resp)
    return (h, resp)
    --return (h, r)

{- | Log in anonymously. -}
loginAnon :: FTPConnection -> IO FTPResult
loginAnon h = login h "anonymous" (Just "anonymous@") Nothing

{- | Log in to an FTP account. -}
login :: FTPConnection                  -- ^ Connection
         -> String                         -- ^ Username
         -> Maybe String                -- ^ Password
         -> Maybe String                -- ^ Account (rarely used)
         -> IO FTPResult
login h user pass acct =
    do
    ur <- sendcmd h ("USER " ++ user)
    if isxresp 300 ur then
       case pass of
            Nothing -> error "FTP server demands password, but no password given"
            Just p -> do pr <- sendcmd h ("PASS " ++ p)
                         if isxresp 300 pr then
                            case acct of
                                Nothing -> error "FTP server demands account, but no account given"
                                Just a -> do ar <- sendcmd h ("ACCT " ++ a)
                                             forceioresp 200 ar
                                             return ar
                            else return $! forcexresp 200 pr
       else return $! forcexresp 200 ur

{- | Sets whether passive mode is used (returns new
connection object reflecting this) -}

setPassive :: FTPConnection -> Bool -> FTPConnection            
setPassive f b = f{isPassive = True}

{- | Establishes a passive connection to the remote. -}

makepasv :: FTPConnection -> 
makspasv h =
    do
    r <- sendcmd("PASV")
    