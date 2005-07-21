{-# LANGUAGE CPP #-}
{- arch-tag: Syslog handler
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
   Module     : MissingH.Logging.Handler.Syslog
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Syslog handler for the Haskell Logging Framework

Written by John Goerzen, jgoerzen\@complete.org

This module implements an interface to the Syslog service commonly
found in Unix\/Linux systems.  This interface is primarily of interest to
developers of servers, as Syslog does not typically display messages in
an interactive fashion.

This module is written in pure Haskell and is capable of logging to a local
or remote machine using the Syslog protocol.

You can create a new Syslog 'LogHandler' by calling 'openlog'.

More information on the Haskell Logging Framework can be found at
"MissingH.Logging.Logger".  This module can also be used outside
of the rest of that framework for those interested in that.
-}

module MissingH.Logging.Handler.Syslog(
                                       -- * Handler Initialization
                                       openlog,
                                       -- * Advanced handler initialization
#ifndef mingw32_HOST_OS
                                       openlog_local,
#endif
                                       openlog_remote,
                                       openlog_generic,
                                       -- * Data Types
                                       Facility(..),
                                       Option(..)
                                       ) where

import MissingH.Logging
import MissingH.Logging.Handler
import Data.Bits
import Network.Socket
import Network.BSD
import List
#ifndef mingw32_HOST_OS
import System.Posix.Process(getProcessID)
#endif
import IO

code_of_pri :: Priority -> Int
code_of_pri p = case p of
                       EMERGENCY -> 0
                       ALERT -> 1
                       CRITICAL -> 2
                       ERROR -> 3
                       WARNING -> 4
                       NOTICE -> 5
                       INFO -> 6
                       DEBUG -> 7

{- | Facilities are used by the system to determine where messages
are sent. -}

data Facility = 
              KERN                      -- ^ Kernel messages; you should likely never use this in your programs
              | USER                    -- ^ General userland messages.  Use this if nothing else is appropriate
              | MAIL                    -- ^ E-Mail system
              | DAEMON                  -- ^ Daemon (server process) messages
              | AUTH                    -- ^ Authentication or security messages
              | SYSLOG                  -- ^ Internal syslog messages; you should likely never use this in your programs
              | LPR                     -- ^ Printer messages
              | NEWS                    -- ^ Usenet news
              | UUCP                    -- ^ UUCP messages
              | CRON                    -- ^ Cron messages
              | AUTHPRIV                -- ^ Private authentication messages
              | LOCAL0                  -- ^ LOCAL0 through LOCAL7 are reserved for you to customize as you wish
              | LOCAL1
              | LOCAL2
              | LOCAL3
              | LOCAL4
              | LOCAL5
              | LOCAL6
              | LOCAL7
                deriving (Eq, Show, Read)

code_of_fac :: Facility -> Int
code_of_fac f = case f of
                       KERN -> 0
                       USER -> 1
                       MAIL -> 2
                       DAEMON -> 3
                       AUTH -> 4
                       SYSLOG -> 5
                       LPR -> 6
                       NEWS -> 7
                       UUCP -> 8
                       CRON -> 9
                       AUTHPRIV -> 10
                       LOCAL0 -> 16
                       LOCAL1 -> 17
                       LOCAL2 -> 18
                       LOCAL3 -> 19
                       LOCAL4 -> 20
                       LOCAL5 -> 21
                       LOCAL6 -> 22
                       LOCAL7 -> 23

makeCode :: Facility -> Priority -> Int
makeCode fac pri =
    let faccode = code_of_fac fac
        pricode = code_of_pri pri in
        (faccode `shiftL` 3) .|. pricode

{- | Options for 'openlog'. -}

data Option = PID                       -- ^ Automatically log process ID (PID) with each message
            | PERROR                    -- ^ Send a copy of each message to stderr
            deriving (Eq,Show,Read)

data SyslogHandler = SyslogHandler {options :: [Option],
                                    facility :: Facility,
                                    identity :: String,
                                    logsocket :: Socket,
                                    address :: SockAddr,
                                    priority :: Priority}

{- | Initialize the Syslog system using the local system's default interface,
\/dev\/log.  Will return a new 'MissingH.Logging.Handler.LogHandler'.

On Windows, instead of using \/dev\/log, this will attempt to send
UDP messages to something listening on the syslog port (514) on localhost.

Use 'openlog_remote' if you need more control.
-}

openlog :: String                       -- ^ The name of this program -- will be prepended to every log message
        -> [Option]                     -- ^ A list of 'Option's.  The list [] is perfectly valid.  ['PID'] is probably most common here.
        -> Facility                     -- ^ The 'Facility' value to pass to the syslog system for every message logged
        -> Priority                     -- ^ Messages logged below this priority will be ignored.  To include every message, set this to 'DEBUG'.
        -> IO SyslogHandler             -- ^ Returns the new handler

#ifdef mingw32_HOST_OS
openlog = openlog_remote AF_INET "localhost" 514
#else
openlog = openlog_local "/dev/log"
#endif

{- | Initialize the Syslog system using an arbitrary Unix socket (FIFO).

Not supported under Windows.
-}

#ifndef mingw32_HOST_OS
openlog_local :: String                 -- ^ Path to FIFO
              -> String                 -- ^ Program name
              -> [Option]               -- ^ 'Option's
              -> Facility               -- ^ Facility value
              -> Priority               -- ^ Priority limit
              -> IO SyslogHandler
openlog_local fifopath ident options fac pri =
    do
    s <- socket AF_UNIX Datagram 0
    openlog_generic s (SockAddrUnix "/dev/log") ident options fac pri
#endif

{- | Log to a remote server via UDP. -}
openlog_remote :: Family                -- ^ Usually AF_INET or AF_INET6; see Network.Socket
               -> HostName              -- ^ Remote hostname.  Some use @localhost@
               -> PortNumber            -- ^ 514 is the default for syslog
               -> String                -- ^ Program name
               -> [Option]              -- ^ 'Option's
               -> Facility              -- ^ Facility value
               -> Priority              -- ^ Priority limit
               -> IO SyslogHandler
openlog_remote fam hostname port ident options fac pri =
    do
    he <- getHostByName hostname
    s <- socket fam Datagram 0
    let addr = SockAddrInet port (head (hostAddresses he))
    openlog_generic s addr ident options fac pri
    
{- | The most powerful initialization mechanism.  Takes an open datagram
socket. -}
openlog_generic :: Socket               -- ^ A datagram socket
                -> SockAddr             -- ^ Address for transmissions
                -> String               -- ^ Program name
                -> [Option]             -- ^ 'Option's
                -> Facility             -- ^ Facility value
                -> Priority             -- ^ Priority limit
                -> IO SyslogHandler
openlog_generic sock addr ident opt fac pri =
    return (SyslogHandler {options = opt,
                            facility = fac,
                            identity = ident,
                            logsocket = sock,
                            address = addr,
                            priority = pri})

instance LogHandler SyslogHandler where
    setLevel sh p = sh{priority = p}
    getLevel sh = priority sh
    emit sh (p, msg) = 
        let code = makeCode (facility sh) p
            getpid :: IO String
            getpid = if (elem PID (options sh))
                     then do
#ifndef mingw32_HOST_OS
                          pid <- getProcessID
#else
                          let pid = "windows"
#endif
                          return ("[" ++ show pid ++ "]")
                     else return ""
                     
            sendstr :: String -> IO String
            sendstr [] = return []
            sendstr omsg = do
                           sent <- sendTo (logsocket sh) omsg (address sh)
                           sendstr (genericDrop sent omsg)
            in
            do
            pidstr <- getpid
            let outstr = "<" ++ (show code) ++ ">" 
                         ++ (identity sh) ++ pidstr ++ ": " ++ msg
            if (elem PERROR (options sh))
               then hPutStrLn stderr outstr
               else return ()
            sendstr (outstr ++ "\0")
            return ()
    close sh = sClose (logsocket sh)

