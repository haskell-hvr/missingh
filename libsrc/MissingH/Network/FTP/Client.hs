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

module MissingH.Network.FTP.Client(-- * Establishing\/Removing connections
                                   easyConnectTo, connectTo,
                                   loginAnon, login, quit,
                                   -- * Configuration
                                   setPassive,
                                   -- * Directory listing
                                   nlst, dir, 
                                   -- * File downloads
                                   getlines, getbinary,
                                   -- * File uploads
                                   putlines, putbinary,
                                   -- * File manipulation
                                   rename, delete, size,
                                   -- * Directory manipulation
                                   cwd, mkdir, rmdir, pwd, 
                                   -- * Low-level advanced commands
                                   FTPConnection(isPassive),
                                   transfercmd, ntransfercmd
                       )
where
import MissingH.Network.FTP.Parser
import Network.BSD
import Network.Socket
import qualified Network
import System.IO
import System.IO.Unsafe
import MissingH.Logging.Logger
import MissingH.Network
import MissingH.Str
data FTPConnection = FTPConnection {readh :: IO String,
                                    readh_internal :: Handle,
                                    writeh :: Handle,
                                    socket_internal :: Socket,
                                    isPassive :: Bool}

{-
getresp h = do c <- hGetContents h
               return (parseGoodReply c)
-}

getresp h = do
            c <- (readh h)
            debugParseGoodReply c


logsend m = debugM "MissingH.Network.FTP.Client" ("FTP sent: " ++ m)
sendcmd h c = do logsend c
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
    s <- connectTCP h p
    r <- socketToHandle s ReadMode
    hSetBuffering r LineBuffering
    w <- socketToHandle s WriteMode
    hSetBuffering w LineBuffering
    let h = FTPConnection {readh = readchars r, 
                           readh_internal = r,
                           socket_internal = s,
                           writeh = w, isPassive = True}
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

{- | Finds the addres sof the remote. -}
makepasv :: FTPConnection -> IO SockAddr
makepasv h =
    do
    r <- sendcmd h "PASV"
    respToSockAddr r

{- | Opens a port and sends it to the remote. -}
makeport :: FTPConnection -> IO (Socket, FTPResult)
makeport h =
    let listenaddr (SockAddrInet _ h) = SockAddrInet aNY_PORT h
        listenaddr _ = error "Can't use port mode to non-TCP server"
        in
        do addr <- getSocketName (socket_internal h)
           mastersock <- listenTCPAddr (listenaddr addr)
           newaddr <- getSocketName mastersock
           result <- sendcmd h ("PORT " ++ toPortString newaddr)
           return (mastersock, result)

{- | Establishes a connection to the remote. 

FIXME: need support for rest
-}
ntransfercmd :: FTPConnection -> String -> IO (Handle, Maybe Integer)
ntransfercmd h cmd =
    let sock = if isPassive h
               then do
                    addr <- makepasv h
                    s <- connectTCPAddr addr
                    return s
               else do 
                    masterresult <- makeport h
                    forceioresp 100 (snd masterresult)
                    acceptres <- accept (fst masterresult)
                    sClose (fst masterresult)
                    return (fst acceptres)
        in do
           s <- sock
           newh <- socketToHandle s ReadWriteMode
           hSetBuffering newh (BlockBuffering (Just 4096))
           r <- sendcmd h cmd
           forceioresp 100 r
           return (newh, Nothing)

{- | Returns the socket part from calling 'ntransfercmd'. -}
transfercmd :: FTPConnection -> String -> IO Handle
transfercmd h cmd = do x <- ntransfercmd h cmd
                       return (fst x)

{- | Stores the lines of data to the remote.  The string gives the
commands to issue. -}
storlines :: FTPConnection -> String -> [String] -> IO FTPResult
storlines h cmd input =
    do
    sendcmd h "TYPE A"
    newh <- transfercmd h cmd
    hPutStr newh (concatMap (++ "\r\n") input)
    hClose newh
    getresp h

{- | Stores the binary data to the remote.  The first string gives the
commands to issue. -}
storbinary :: FTPConnection -> String -> String -> IO FTPResult
storbinary h cmd input =
    do sendcmd h "TYPE I"
       newh <- transfercmd h cmd
       hPutStr newh input
       hClose newh
       getresp h

{- | Retrieves lines of data from the remote. The string gives 
the command to issue. -}
retrlines :: FTPConnection -> String -> IO ([String], FTPResult)
retrlines h cmd =
    -- foo returns the empty last item and closes the handle when done
    let foo theh [] = do hClose theh
                         r <- getresp h
                         return ([], r)
        foo theh ("" : []) = foo theh []
        foo theh (x:xs) = do next <- unsafeInterleaveIO $ foo theh xs
                             return $ (x : fst next, snd next)
        in do
              sendcmd h "TYPE A"
              newh <- transfercmd h cmd
              c <- hGetContents newh
              foo newh (split "\r\n" $ c)

{- | Retrieves binary data from the remote. The string gives the command
to issue. -}
retrbinary :: FTPConnection -> String -> IO (String, FTPResult)
retrbinary h cmd =
    let foo h2 [] = do hClose h2
                       r <- getresp h
                       return ([], r)
        foo h2 (x:xs) = do next <- unsafeInterleaveIO $ foo h2 xs
                           return $ (x : fst next, snd next)
        in do
           sendcmd h "TYPE I"
           newh <- transfercmd h cmd
           c <- hGetContents newh
           foo newh c

{- | Retrieves the specified file in text mode. -}
getlines :: FTPConnection -> String -> IO ([String], FTPResult)
getlines h fn = retrlines h ("RETR " ++ fn)

{- | Retrieves the specified file in binary mode. -}
getbinary :: FTPConnection -> String -> IO (String, FTPResult)
getbinary h fn = retrbinary h ("RETR " ++ fn)

{- | Puts data in the specified file in text mode.  The first string
is the filename. -}
putlines :: FTPConnection -> String -> [String] -> IO FTPResult
putlines h fn input = storlines h ("STOR " ++ fn) input 

{- | Puts data in the specified file in binary.  Ths first string is the filename. -}
putbinary :: FTPConnection -> String -> String -> IO FTPResult
putbinary h fn input = storbinary h ("STOR " ++ fn) input 

{- | Retrieves a list of files in the given directory. 

FIXME: should this take a list of dirs? -}
nlst :: FTPConnection
        -> Maybe String                 -- ^ The directory to list.  If Nothing, list the current directory.
        -> IO [String]
nlst h Nothing = retrlines h "NLST" >>= return . fst
nlst h (Just dirname) = retrlines h ("NLST " ++ dirname) >>= return . fst

{- | Retrieve the system-specific long form of a directory list.

FIXME: should this take a list of dirs? -}
dir :: FTPConnection
       -> Maybe String                  -- ^ The directory to list.  If Nothing, list the current directory.
       -> IO [String]
dir h Nothing = retrlines h "LIST" >>= return .  fst
dir h (Just dirname) = retrlines h ("LIST " ++ dirname) >>= return . fst

{- | Rename or move a file. -}
rename :: FTPConnection
          -> String                     -- ^ Old name
          -> String                     -- ^ New name
          -> IO FTPResult
rename h old new = do
                   r <- sendcmd h ("RNFR " ++ old)
                   forceioresp 300 r
                   sendcmd h ("RNTO " ++ new)

{- | Delete (unlink) a file. -}
delete :: FTPConnection -> String -> IO FTPResult
delete h fn = sendcmd h ("DELE " ++ fn)

{- | Change the working directory. -}
cwd :: FTPConnection -> String -> IO FTPResult
cwd h ".." = sendcmd h "CDUP"
cwd h "" = cwd h "."
cwd h newdir = sendcmd h ("CWD " ++ newdir)

{- | Get the size of a file.

This command is non-standard and may possibly fail.
-}
size :: (Num a, Read a) => FTPConnection -> String -> IO a
size h fn = do
            r <- sendcmd h ("SIZE " ++ fn)
            forceioresp 200 r
            return (read . head . snd $ r)

-- | Make new directory.  Returns the absolute name of the
-- new directory if possible.
mkdir :: FTPConnection -> String -> IO (Maybe String, FTPResult)
mkdir h fn = do x <- sendcmd h ("MKD " ++ fn)
                return (parseDirName x, x)

-- | Remove a directory.
rmdir :: FTPConnection -> String -> IO FTPResult
rmdir h fn = sendcmd h ("RMD " ++ fn)

-- | Print the current working directory.  The first component of the result
-- is the parsed directory name, if the servers response was parsable.
pwd :: FTPConnection -> IO (Maybe String, FTPResult)
pwd h = do x <- sendcmd h ("PWD")
           return (parseDirName x, x)

-- | Log off the server and quit.
quit :: FTPConnection -> IO FTPResult
quit h = do
         r <- sendcmd h "QUIT"
         hClose (writeh h)
         -- hClose (readh_internal h)
         return r
