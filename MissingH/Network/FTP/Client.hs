{- arch-tag: FTP client support
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
   Module     : MissingH.Network.FTP.Client
   Copyright  : Copyright (C) 2004-2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : experimental
   Portability: systems with networking

This module provides a client-side interface to the File Transfer Protocol
as defined by RFC959 and RFC1123.

Written by John Goerzen, jgoerzen\@complete.org

Welcome to the FTP module for Haskell.

Here is a quick usage example to get you started.  This is a log of a real
session with ghci: 

(This would be similar in a "do" block.  You could also save it to a file and
run that with Hugs.)

> Prelude> :l MissingH.Network.FTP.Client
> ...

The above loads the module.

Next, we enable the debugging.  This will turn on all the @FTP sent@ and
@FTP received@ messages you'll see.

> Prelude MissingH.Network.FTP.Client> enableFTPDebugging

Now, connect to the server on @ftp.kernel.org@.

> *MissingH.Network.FTP.Client> h <- easyConnectFTP "ftp.kernel.org"
> FTP received: 220 Welcome to ftp.kernel.org.

And log in anonymously.

> *MissingH.Network.FTP.Client> loginAnon h
> FTP sent: USER anonymous
> FTP received: 331 Please specify the password.
> FTP sent: PASS anonymous@
> ...
> FTP received: 230 Login successful.

Change the directory...

> Prelude MissingH.Network.FTP.Client> cwd h "/pub/linux/kernel/Historic"
> FTP sent: CWD /pub/linux/kernel/Historic
> FTP received: 250 Directory successfully changed.

Let's look at the directory. 'nlst' returns a list of strings, each string
corresponding to a filename.  Here, @putStrLn . unlines@ will simply
print them out, one per line.

> Prelude MissingH.Network.FTP.Client> nlst h Nothing >>= putStrLn . unlines
> FTP sent: TYPE A
> FTP received: 200 Switching to ASCII mode.
> FTP sent: PASV
> FTP received: 227 Entering Passive Mode (204,152,189,116,130,143)
> FTP sent: NLST
> FTP received: 150 Here comes the directory listing.
> linux-0.01.tar.bz2
> linux-0.01.tar.bz2.sign
> linux-0.01.tar.gz
> linux-0.01.tar.gz.sign
> linux-0.01.tar.sign
> old-versions
> v0.99
> FTP received: 226 Directory send OK.

Let's try downloading something and print it to the screen.  Again, we use
@putStrLn@.  We use @fst@ here because 'getbinary' returns a tuple consisting
of a string representing the data and a 'FTPResult' code.

> Prelude MissingH.Network.FTP.Client> getbinary h "linux-0.01.tar.gz.sign" >>= putStrLn . fst
> FTP sent: TYPE I
> FTP received: 200 Switching to Binary mode.
> FTP sent: PASV
> FTP received: 227 Entering Passive Mode (204,152,189,116,121,121)
> FTP sent: RETR linux-0.01.tar.gz.sign
> FTP received: 150 Opening BINARY mode data connection for linux-0.01.tar.gz.sign (248 bytes).
> -----BEGIN PGP SIGNATURE-----
> Version: GnuPG v1.0.0 (GNU/Linux)
> Comment: See http://www.kernel.org/signature.html for info
> 
> iD8DBQA54rf0yGugalF9Dw4RAqelAJ9lafFni4f/QyJ2IqDXzW2nz/ZIogCfRPtg
> uYpWffOhkyByfhUt8Lcelec=
> =KnLA
> -----END PGP SIGNATURE-----
> FTP received: 226 File send OK.

Here's an example showing you what the result code looks like.

> Prelude MissingH.Network.FTP.Client> getbinary h "linux-0.01.tar.gz.sign" >>= print . snd
> ...
> (226,["File send OK."])

The first component of the 'FTPResult' object is the numeric status code from
the server.  The second component is a list of message lines from the server.

Now, let's get a more detailed directory listing:

> Prelude MissingH.Network.FTP.Client> dir h Nothing >>= putStrLn . unlines
> ...
> -r--r--r--    1 536      536         63362 Oct 30  1993 linux-0.01.tar.bz2
> -r--r--r--    1 536      536           248 Oct 30  1993 linux-0.01.tar.bz2.sign
> -r--r--r--    1 536      536         73091 Oct 30  1993 linux-0.01.tar.gz
> -r--r--r--    1 536      536           248 Oct 30  1993 linux-0.01.tar.gz.sign
> -r--r--r--    1 536      536           248 Oct 30  1993 linux-0.01.tar.sign
> drwxrwsr-x    5 536      536          4096 Mar 20  2003 old-versions
> drwxrwsr-x    2 536      536          4096 Mar 20  2003 v0.99
> FTP received: 226 Directory send OK.

And finally, log out:

> Prelude MissingH.Network.FTP.Client> quit h
> FTP sent: QUIT
> FTP received: 221 Goodbye.

Here is one big important caution:

/You MUST consume all data from commands that return file data before you
issue any other FTP commands./

That's due to the lazy nature of Haskell.  This means that, for instance,
you can't just iterate over the items 'nlst' returns, trying to 'getbinary'
each one of them -- the system is still transferring 'nlst' data while you
are trying that, and confusion will ensue.  Either open two FTP connections
or make sure you consume the 'nlst' data first.

Here is a partial list of commands effected: 'nlst', 'dir', 'getbinary',
'getlines', 'downloadbinary'.

The 'MissingH.List.seqList' function could be quite helpful here.  For instance:

> x <- nlst h Nothing
> map (\fn -> ...download files from FTP... ) (seqList x)

If you omit the call to 'MissingH.List.seqList', commands to download files
will be issued before the entire directory listing is read.  FTP cannot handle
this.

The corrolary is:

/Actions that yield lazy data for data uploading must not issue FTP
commands themselves./

This will be fairly rare.  Just be aware of this.

This module logs messages under @MissingH.Network.FTP.Client@ for outgoing
traffic and @MissingH.Network.FTP.ParserClient@ for incoming traffic, all with the
'MissingH.Logging.DEBUG' priority, so by default, no log messages are seen.
The 'enableFTPDebugging' function will adjust the priorities of these
two handlers so debug messages are seen.  Only control channel conversations
are logged.  Data channel conversations are never logged.

All exceptions raised by this module have a string beginning with
@\"FTP: \"@.  Most errors will be IO userErrors.  In a few extremely rare
cases, errors may be raised by the Prelude error function, though these
will also have a string beginning with @\"FTP: \"@.  Exceptions raised by
the underlying networking code will be passed on to you unmodified.

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
                                   easyConnectFTP, connectFTP,
                                   loginAnon, login, quit, 
                                   -- * Configuration
                                   setPassive, isPassive, enableFTPDebugging,
                                   -- * Directory listing
                                   nlst, dir, 
                                   -- * File downloads
                                   getlines, getbinary,
                                   downloadbinary,
                                   -- * File uploads
                                   putlines, putbinary,
                                   uploadbinary,
                                   -- * File manipulation
                                   rename, delete, size,
                                   -- * Directory manipulation
                                   cwd, mkdir, rmdir, pwd, 
                                   -- * Low-level advanced commands
                                   FTPConnection,
                                   transfercmd, ntransfercmd,
                                   retrlines, storlines
                       )
where
import MissingH.Network.FTP.ParserClient
import Network.BSD
import Network.Socket
import MissingH.IO.Binary
import qualified Network
import System.IO
import System.IO.Unsafe
import MissingH.Logging.Logger
import MissingH.Network
import MissingH.Str
data FTPConnection = FTPConnection {readh :: IO String,
                                    writeh :: Handle,
                                    socket_internal :: Socket,
                                    isPassive :: Bool}

getresp h = do
            c <- (readh h)
            debugParseGoodReply c


logsend m = debugM "MissingH.Network.FTP.Client" ("FTP sent: " ++ m)
sendcmd h c = do logsend c
                 hPutStr (writeh h) (c ++ "\r\n")
                 getresp h

{- | Connect to the remote FTP server and read but discard
   the welcome.  Assumes
   default FTP port, 21, on remote. -}
easyConnectFTP :: Network.HostName -> IO FTPConnection
easyConnectFTP h = do x <- connectFTP h 21
                      return (fst x)

{- | Enable logging of FTP messages through 'MissingH.Logging.Logger'.
This sets the log levels of @MissingH.Network.FTP.ParserClient@ and
@MissingH.Network.FTP.Client@ to DEBUG.  By default, this means that
full protocol dumps will be sent to stderr.

The effect is global and persists until changed.
-}
enableFTPDebugging :: IO ()
enableFTPDebugging = 
    do
    updateGlobalLogger "MissingH.Network.FTP.ParserClient" (setLevel DEBUG)
    updateGlobalLogger "MissingH.Network.FTP.Client" (setLevel DEBUG)

{- | Connect to remote FTP server and read the welcome. -}
connectFTP :: Network.HostName -> PortNumber -> IO (FTPConnection, FTPResult)
connectFTP h p =
    let readchars :: Handle -> IO String
        readchars h = do
                      c <- hGetChar h
                      next <- unsafeInterleaveIO $ readchars h
                      return (c : next)
        in
    do
    s <- connectTCP h p
    newh <- socketToHandle s ReadWriteMode
    hSetBuffering newh LineBuffering
    let h = FTPConnection {readh = readchars newh, 
                           socket_internal = s,
                           writeh = newh, isPassive = True}
    resp <- getresp h
    forceioresp 200 resp
    return (h, resp)

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
            Nothing -> fail "FTP: Server demands password, but no password given"
            Just p -> do pr <- sendcmd h ("PASS " ++ p)
                         if isxresp 300 pr then
                            case acct of
                                Nothing -> fail "FTP: server demands account, but no account given"
                                Just a -> do ar <- sendcmd h ("ACCT " ++ a)
                                             forceioresp 200 ar
                                             return ar
                            else return $! forcexresp 200 pr
       else return $! forcexresp 200 ur

{- | Sets whether passive mode is used (returns new
connection object reflecting this) -}
setPassive :: FTPConnection -> Bool -> FTPConnection            
setPassive f b = f{isPassive = b}

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
        listenaddr _ = error "FTP: Can't use port mode to non-TCP server"
        in
        do addr <- getSocketName (socket_internal h)
           mastersock <- listenTCPAddr (listenaddr addr) 1
           newaddr <- getSocketName mastersock
           ps <- toPortString newaddr
           result <- sendcmd h ("PORT " ++ ps)
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
                    r <- sendcmd h cmd
                    forceioresp 100 r
                    return s
               else do 
                    masterresult <- makeport h
                    r <- sendcmd h cmd
                    forceioresp 100 r
                    acceptres <- accept (fst masterresult)
                    sClose (fst masterresult)
                    return (fst acceptres)
        in do
           s <- sock
           newh <- socketToHandle s ReadWriteMode
           hSetBuffering newh (BlockBuffering (Just 4096))
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

{- | Puts data in the specified file in binary.  The first string is the filename. -}
putbinary :: FTPConnection -> String -> String -> IO FTPResult
putbinary h fn input = storbinary h ("STOR " ++ fn) input 

{- | Uploads a file from disk in binary mode. Note: filename is used for both local and remote. -}
uploadbinary :: FTPConnection -> String -> IO FTPResult
uploadbinary h fn = do input <- readBinaryFile fn
                       putbinary h fn input

{- | Downloads a file from remote and saves to disk in binary mode.  Note: filename is used for both local and remote. -}
downloadbinary :: FTPConnection -> String -> IO FTPResult
downloadbinary h fn = do r <- getbinary h fn
                         writeBinaryFile fn (fst r)
                         return (snd r)

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
