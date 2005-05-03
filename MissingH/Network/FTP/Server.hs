{- arch-tag: FTP server support
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
   Module     : MissingH.Network.FTP.Server
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : experimental
   Portability: systems with networking

This module provides a server-side interface to the File Transfer Protocol
as defined by:

 * RFC959, basic protocol

 * RFC1123, clarifications

 * RFC1579, passive mode discussion

Written by John Goerzen, jgoerzen\@complete.org

This is a modular FTP server implementation in pure Haskell.  It is highly
adaptable to many different tasks, and can serve up not only real files
and directories, but also virtually any data structure you could represent
as a filesystem.  It does this by using the
"MissingH.IO.HVFS" and "MissingH.IO.HVIO" modules.

In addition, basic networking and multitasking configuration is handled
via "MissingH.Network.SocketServer" and logging via 
"MissingH.Logging.Logger".

This module is believed to be secure, but it not believed to be robust enough
for use on a public FTP server.  In particular, it may be vulnerable to denial
of service attacks due to no timeouts or restrictions on data size, and
error catching is not yet completely pervasive.  These will be fixed in time.
Your patches would also be welcomed.

Here is an example server that serves up the entire local filesystem
in a read-only manner:

>import MissingH.Network.FTP.Server
>import MissingH.Network.SocketServer
>import MissingH.Logging.Logger
>import MissingH.IO.HVFS
>import MissingH.IO.HVFS.Combinators
>
>main = do
>       updateGlobalLogger "" (setLevel DEBUG)
>       updateGlobalLogger "MissingH.Network.FTP.Server" (setLevel DEBUG)
>       let opts = (simpleTCPOptions 12345) {reuse = True}
>       serveTCPforever opts $
>            threadedHandler $
>            loggingHandler "" INFO $
>            handleHandler $
>            anonFtpHandler (HVFSReadOnly SystemFS)

Hint: if you wantto serve up only part of a filesystem, see
'MissingH.IO.HVFS.Combinators.newHVFSChroot'.
-}

module MissingH.Network.FTP.Server(
                                   anonFtpHandler
                                  )
where
import MissingH.Network.FTP.ParserServer
import MissingH.Network.FTP.ParserClient
import Network.BSD
import Network.Socket
import qualified Network
import System.IO
import System.IO.Error
import MissingH.Logging.Logger
import MissingH.Network
import MissingH.Network.SocketServer
import MissingH.Str
import MissingH.Printf
import MissingH.IO.HVIO
import MissingH.IO.HVFS
import MissingH.IO.HVFS.InstanceHelpers
import MissingH.IO.HVFS.Utils
import Data.Char
import MissingH.Printf
import Data.IORef
import Data.List
import Control.Exception(finally)
import MissingH.IO

data DataType = ASCII | Binary
              deriving (Eq, Show)
data AuthState = NoAuth 
              | User String
              | Authenticated String
                deriving (Eq, Show)
data DataChan = NoChannel
              | PassiveMode SocketServer
              | PortMode SockAddr
data FTPState = FTPState
              { auth :: IORef AuthState,
                datatype :: IORef DataType,
                rename :: IORef (Maybe String),
                datachan :: IORef DataChan,
                local :: SockAddr,
                remote :: SockAddr}

data FTPServer = forall a. HVFSOpenable a => FTPServer Handle a FTPState

s_crlf = "\r\n"
logname = "MissingH.Network.FTP.Server"
ftpPutStrLn :: FTPServer -> String -> IO ()
ftpPutStrLn (FTPServer h _ _) text =
    do hPutStr h (text ++ s_crlf)
       hFlush h

{- | Send a reply code, handling multi-line text as necessary. -}
sendReply :: FTPServer -> Int -> String -> IO ()
sendReply h codei text =
    let codes = vsprintf "%03d" codei
        writethis [] = ftpPutStrLn h (codes ++ "  ")
        writethis [item] = ftpPutStrLn h (codes ++ " " ++ item)
        writethis (item:xs) = do ftpPutStrLn h (codes ++ "-" ++ item)
                                 writethis xs
        in 
        writethis (map (rstrip) (lines text))

{- | Main FTP handler; pass the result of applying this to one argument to 
'MissingH.Network.SocketServer.handleHandler' -}

anonFtpHandler :: forall a. HVFSOpenable a => a -> Handle -> SockAddr -> SockAddr -> IO ()
anonFtpHandler f h saremote salocal =
    let serv r = FTPServer h f r
        in
        traplogging logname NOTICE "" $
          do authr <- newIORef (NoAuth)
             typer <- newIORef ASCII
             renamer <- newIORef (Nothing::Maybe String)
             chanr <- newIORef (NoChannel)
             let s = serv (FTPState {auth = authr, datatype = typer,
                                    rename = renamer, datachan = chanr,
                                    local = salocal, remote = saremote})
             sendReply s 220 "Welcome to MissingH.Network.FTP.Server."
             commandLoop s

type CommandHandler = FTPServer -> String -> IO Bool
type Command = (String, (CommandHandler, (String, String)))

instance Eq Command where
    x == y = (fst x) == (fst y)
instance Ord Command where
    compare x y = compare (fst x) (fst y)

trapIOError :: FTPServer -> IO a -> (a -> IO Bool) -> IO Bool
trapIOError h testAction remainingAction =
    do result <- try testAction
       case result of
         Left err -> do sendReply h 550 (show err)
                        return True
         Right result -> remainingAction result

forceLogin :: CommandHandler -> CommandHandler
forceLogin func h@(FTPServer _ _ state) args =
    do state <- readIORef (auth state)
       case state of 
          Authenticated _ -> func h args
          x -> do sendReply h 530 "Command not possible in non-authenticated state."
                  return True

commands :: [Command]
commands =
    [("HELP", (cmd_help,             help_help))
    ,("QUIT", (cmd_quit,             help_quit))
    ,("USER", (cmd_user,             help_user))
    ,("PASS", (cmd_pass,             help_pass))
    ,("CWD",  (forceLogin cmd_cwd,   help_cwd))
    ,("CDUP", (forceLogin cmd_cdup,  help_cdup))
    ,("TYPE", (forceLogin cmd_type,  help_type))
    ,("NOOP", (forceLogin cmd_noop,  help_noop))
    ,("RNFR", (forceLogin cmd_rnfr,  help_rnfr))
    ,("RNTO", (forceLogin cmd_rnto,  help_rnto))
    ,("DELE", (forceLogin cmd_dele,  help_dele))
    ,("RMD",  (forceLogin cmd_rmd,   help_rmd))
    ,("MKD",  (forceLogin cmd_mkd,   help_mkd))
    ,("PWD",  (forceLogin cmd_pwd,   help_pwd))
    ,("MODE", (forceLogin cmd_mode,  help_mode))
    ,("STRU", (forceLogin cmd_stru,  help_stru))
    ,("PASV", (forceLogin cmd_pasv,  help_pasv))
    ,("PORT", (forceLogin cmd_port,  help_port))
    ,("RETR", (forceLogin cmd_retr,  help_retr))
    ,("STOR", (forceLogin cmd_stor,  help_stor))
    ,("STAT", (forceLogin cmd_stat,  help_stat))
    ,("SYST", (forceLogin cmd_syst,  help_syst))
    ,("NLST", (forceLogin cmd_nlst,  help_nlst))
    ,("LIST", (forceLogin cmd_list,  help_list))
    ]

commandLoop :: FTPServer -> IO ()
commandLoop h@(FTPServer fh _ _) =
    let errorhandler e = do noticeM logname
                                    ("Closing due to error: " ++ (show e))
                            hClose fh
                            return False
        in do continue <- (flip catch) errorhandler 
               (do x <- parseCommand fh
                   case x of
                     Left err -> do sendReply h 500 $
                                      " Couldn't parse command: " ++ (show err)
                                    return True
                     Right (cmd, args) -> 
                         case lookup cmd commands of
                            Nothing -> do sendReply h 502 $
                                           "Unrecognized command " ++ cmd
                                          return True
                            Just hdlr -> (fst hdlr) h args
               )
              if continue
                 then commandLoop h
                 else return ()

help_quit =
    ("Terminate the session",
     "")

cmd_quit :: CommandHandler
cmd_quit h args =
    do sendReply h 221 "OK, Goodbye."
       return False

help_user =
    ("Provide a username",
     unlines $ 
     ["USER username will provide the username for authentication."
     ,"It should be followed by a PASS command to finish the authentication."
     ])

cmd_user :: CommandHandler
cmd_user h@(FTPServer _ _ state) passedargs =
    let args = strip passedargs
        in
        case args of
           "anonymous" -> do sendReply h 331 "User name accepted; send password."
                             writeIORef (auth state) (User args)
                             return True
           _ -> do sendReply h 530 "Unrecognized user name; please try \"anonymous\""
                   writeIORef (auth state) NoAuth
                   return True

help_pass =
    ("Provide a password",
     "PASS password will provide the password for authentication.")
cmd_pass :: CommandHandler
cmd_pass h@(FTPServer _ _ state) passedargs =
    do curstate <- readIORef (auth state)
       case curstate of
         User "anonymous" -> 
             do sendReply h 230 "Anonymous login successful."
                writeIORef (auth state) (Authenticated "anonymous")
                infoM logname "Anonymous authentication successful"
                return True
         _ -> do sendReply h 530 "Out of sequence PASS command"
                 return True

help_cwd =
    ("Change working directory",
     unlines $
     ["Syntax: CWD cwd"
     ,""
     ,"Changes the working directory to the specified item"])

cmd_cwd :: CommandHandler
cmd_cwd h@(FTPServer _ fs _) args =
    do trapIOError h (vSetCurrentDirectory fs args)
         $ \_ -> do
                 newdir <- vGetCurrentDirectory fs
                 sendReply h 250 $ "New directory now " ++ newdir
                 return True

help_cdup = 
    ("Change to parent directory", "Same as CWD ..")
cmd_cdup h _ = cmd_cwd h ".."

help_type =
    ("Change the type of data transfer", "Valid args are A, AN, and I")
cmd_type :: CommandHandler
cmd_type h@(FTPServer _ _ state) args =
    let changetype newt =
            do oldtype <- readIORef (datatype state)
               writeIORef (datatype state) newt
               sendReply h 200 $ "Type changed from " ++ show oldtype ++
                             " to " ++ show newt
               return True
        in case args of
         "I" -> changetype Binary
         "L 8" -> changetype Binary
         "A" -> changetype ASCII
         "AN" -> changetype ASCII
         "AT" -> changetype ASCII
         _ -> do sendReply h 504 $ "Type \"" ++ args ++ "\" not supported."
                 return True

closeconn :: FTPServer -> IO ()
closeconn h@(FTPServer _ _ state) =
    do dc <- readIORef (datachan state)
       writeIORef (datachan state) NoChannel

help_port = ("Initiate a port-mode connection", "")
cmd_port :: CommandHandler
cmd_port h@(FTPServer _ _ state) args =
    let doIt clientsa = 
            do writeIORef (datachan state) (PortMode clientsa)
               str <- showSockAddr clientsa
               sendReply h 200 $ "OK, later I will connect to " ++ str
               return True
        in
        do closeconn h                      -- Close any existing connection
           trapIOError h (fromPortString args) $  (\clientsa -> 
               case clientsa of
                   SockAddrInet _ ha -> 
                      case (local state) of
                          SockAddrInet _ ha2 -> if ha /= ha2
                                                  then do sendReply h 501 "Will only connect to same client as command channel."
                                                          return True
                                                  else doIt clientsa
                          _ -> do sendReply h 501 "Require IPv4 on client"
                                  return True
                   _ -> do sendReply h 501 "Require IPv4 in specified address"
                           return True
                                                   )

runDataChan :: FTPServer -> (FTPServer -> Socket -> IO ()) -> IO ()
runDataChan h@(FTPServer _ _ state) func =
    do chan <- readIORef (datachan state)
       case chan of
          NoChannel -> fail "Can't connect when no data channel exists"
          PassiveMode ss -> do finally (handleOne ss (\sock _ _ -> func h sock))
                                       (do closeSocketServer ss
                                           closeconn h
                                       )
          PortMode sa -> do proto <- getProtocolNumber "tcp"
                            s <- socket AF_INET Stream proto
                            connect s sa
                            finally (func h s) $ closeconn h

help_pasv = ("Initiate a passive-mode connection", "")
cmd_pasv :: CommandHandler
cmd_pasv h@(FTPServer _ _ state) args =
    do closeconn h                      -- Close any existing connection
       addr <- case (local state) of 
                    (SockAddrInet _ ha) -> return ha
                    _ -> fail "Require IPv4 sockets"
       let ssopts = InetServerOptions 
                    { listenQueueSize = 1,
                      portNumber = aNY_PORT,
                      interface = addr,
                      reuse = False,
                      family = AF_INET,
                      sockType = Stream,
                      protoStr = "tcp"
                    }
       ss <- setupSocketServer ssopts
       sa <- getSocketName (sockSS ss)
       portstring <- toPortString sa
       sendReply h 227 $ "Entering passive mode (" ++ portstring ++ ")"
       writeIORef (datachan state) (PassiveMode ss)
       return True
                 
                                        
       
help_noop = ("Do nothing", "")
cmd_noop :: CommandHandler
cmd_noop h _ =
    do sendReply h 200 "OK"
       return True

help_rnfr = ("Specify FROM name for a file rename", "")
cmd_rnfr :: CommandHandler
cmd_rnfr h@(FTPServer _ _ state) args = 
    if length args < 1
       then do sendReply h 501 "Filename required"
               return True
       else do writeIORef (rename state) (Just args)
               sendReply h 350 "Noted rename from name; please send RNTO."
               return True

help_stor = ("Upload a file", "")
cmd_stor :: CommandHandler
cmd_stor h@(FTPServer _ fs state) args =
    let datamap :: [String] -> [String]
        datamap instr =
            let linemap :: String -> String
                linemap x = if endswith "\r" x
                              then take ((length x) - 1) x
                              else x
                in map linemap instr
        runit fhencap _ sock =
            case fhencap of
              HVFSOpenEncap fh ->
                  do readh <- socketToHandle sock ReadMode
                     mode <- readIORef (datatype state)
                     case mode of
                      ASCII -> finally (hLineInteract readh fh datamap)
                                       (hClose readh)
                      Binary -> finally (do vSetBuffering fh (BlockBuffering (Just 4096))
                                            hCopy readh fh
                                        ) (hClose readh)
        in
        if length args < 1
           then do sendReply h 501 "Filename required"
                   return True
           else trapIOError h (vOpen fs args WriteMode) 
                  (\fhencap ->
                    trapIOError h (do sendReply h 150 "File OK; about to open data channel"
                                      runDataChan h (runit fhencap)
                                  )
                                  (\_ ->
                                   do case fhencap of
                                        HVFSOpenEncap fh -> vClose fh
                                      sendReply h 226 "Closing data connection; transfer complete."
                                      return True
                                   )
                   )

rtransmitString :: String -> FTPServer  -> Socket -> IO ()
rtransmitString thestr (FTPServer _ _ state) sock =
    let fixlines :: [String] -> [String]
        fixlines x = map (\y -> y ++ "\r") x
        copyit h =
            hPutStr h $ unlines . fixlines . lines $ thestr
        in
        do writeh <- socketToHandle sock WriteMode
           hSetBuffering writeh (BlockBuffering (Just 4096))
           mode <- readIORef (datatype state)
           case mode of
              ASCII -> finally (copyit writeh)
                               (hClose writeh)
              Binary -> finally (hPutStr writeh thestr)
                                (hClose writeh)

rtransmitH :: HVFSOpenEncap -> FTPServer -> Socket -> IO ()
rtransmitH fhencap h sock =
    case fhencap of
       HVFSOpenEncap fh ->
        finally (do c <- vGetContents fh
                    rtransmitString c h sock
                ) (vClose fh)

genericTransmit :: FTPServer -> a -> (a -> FTPServer -> Socket -> IO ()) -> IO Bool
genericTransmit h dat func =
    trapIOError h 
      (do sendReply h 150 "I'm going to open the data channel now."
          runDataChan h (func dat)
      ) (\_ ->
               do sendReply h 226 "Closing data connection; transfer complete."
                  return True
        )

genericTransmitHandle :: FTPServer -> HVFSOpenEncap -> IO Bool
genericTransmitHandle h dat =
    genericTransmit h dat rtransmitH

genericTransmitString :: FTPServer -> String -> IO Bool
genericTransmitString h dat =
    genericTransmit h dat rtransmitString


help_retr = ("Retrieve a file", "")
cmd_retr :: CommandHandler
cmd_retr h@(FTPServer _ fs state) args =
        if length args < 1
           then do sendReply h 501 "Filename required"
                   return True
           else trapIOError h (vOpen fs args ReadMode) 
                    (\fhencap -> genericTransmitHandle h fhencap)

help_rnto = ("Specify TO name for a file name", "")
cmd_rnto :: CommandHandler
cmd_rnto h@(FTPServer _ fs state) args =
    if length args < 1
       then do sendReply h 501 "Filename required"
               return True
       else do fr <- readIORef (rename state)
               case fr of
                   Nothing -> do sendReply h 503 "RNFR required before RNTO"
                                 return True
                   Just fromname -> 
                       do writeIORef (rename state) Nothing
                          trapIOError h (vRenameFile fs fromname args)
                              $ \_ -> do sendReply h 250 
                                           ("File " ++ fromname ++ 
                                            " renamed to " ++ args)
                                         return True

help_dele = ("Delete files", "")
cmd_dele :: CommandHandler
cmd_dele h@(FTPServer _ fs _) args =
    if length args < 1
       then do sendReply h 501 "Filename required"
               return True
       else trapIOError h (vRemoveFile fs args) $
              \_ -> do sendReply h 250 $ "File " ++ args ++ " deleted."
                       return True

help_nlst = ("Get plain listing of files", "")
cmd_nlst :: CommandHandler
cmd_nlst h@(FTPServer _ fs _) args =
    let fn = case args of
                       "" -> "."
                       x -> x
        in
        trapIOError h (vGetDirectoryContents fs fn)
           (\l -> genericTransmitString h (unlines l))

help_list = ("Get an annotated listing of files", "")
cmd_list :: CommandHandler
cmd_list h@(FTPServer _ fs _) args =
    let fn = case args of
                       "" -> "."
                       x -> x
        in
        trapIOError h (lsl fs fn)
                    (\l -> genericTransmitString h l)

help_rmd = ("Remove directory", "")
cmd_rmd :: CommandHandler
cmd_rmd h@(FTPServer _ fs _) args =
    if length args < 1
       then do sendReply h 501 "Filename required"
               return True
       else trapIOError h (vRemoveDirectory fs args) $
            \_ -> do sendReply h 250 $ "Directory " ++ args ++ " removed."
                     return True

help_mkd = ("Make directory", "")
cmd_mkd :: CommandHandler
cmd_mkd h@(FTPServer _ fs _) args =
    if length args < 1
       then do sendReply h 501 "Filename required"
               return True
       else trapIOError h (vCreateDirectory fs args) $
            \_ -> do newname <- getFullPath fs args
                     sendReply h 257 $ "\"" ++ newname ++ "\" created."
                     return True

help_pwd = ("Print working directory", "")
cmd_pwd :: CommandHandler
cmd_pwd h@(FTPServer _ fs _) _ =
    do d <- vGetCurrentDirectory fs
       sendReply h 257 $ "\"" ++ d ++ "\" is the current working directory."
       return True

help_mode = ("Provided for compatibility only", "")
cmd_mode :: CommandHandler
cmd_mode h args =
    case args of
        "S" -> do sendReply h 200 "Mode is Stream."
                  return True
        x -> do sendReply h 504 $ "Mode \"" ++ x ++ "\" not supported."
                return True

help_stru = ("Provided for compatibility only", "")
cmd_stru :: CommandHandler
cmd_stru h args =
    case args of
        "F" -> do sendReply h 200 "Structure is File."
                  return True
        x -> do sendReply h 504 $ "Structure \"" ++ x ++ "\" not supported."
                return True

help_syst = ("Display system type", "")
cmd_syst :: CommandHandler
cmd_syst h _ =
    -- I have no idea what this L8 means, but everyone else seems to do
    -- this, so I do too..
    do sendReply h 215 "UNIX Type: L8"
       return True

help_stat = ("Display sever statistics", "")
cmd_stat :: CommandHandler
cmd_stat h@(FTPServer _ _ state) _ =
    do loc <- showSockAddr (local state)
       rem <- showSockAddr (remote state)
       auth <- readIORef (auth state)
       datm <- readIORef (datatype state)
       sendReply h 211 $ unlines $
         [" *** Sever statistics and information"
         ," *** Please type HELP for more details"
         ,""
         ,"Server Software     : MissingH, http://quux.org/devel/missingh"
         ,"Connected From      : " ++ rem
         ,"Connected To        : " ++ loc
         ,"Data Transfer Type  : " ++ (show datm)
         ,"Auth Status         : " ++ (show auth)
         ,"End of status."]
       return True
          

help_help =
    ("Display help on available commands",
     "When called without arguments, shows a summary of available system\n"
     ++ "commands.  When called with an argument, shows detailed information\n"
     ++ "on that specific command.")

cmd_help :: CommandHandler
cmd_help h@(FTPServer _ _ state) args =
    let genericreply addr = unlines $
          [" --- General Help Response ---"
          ,""
          ,"Welcome to the FTP server, " ++ addr ++ "."
          ,"This server is implemented as the MissingH.Network.FTP.Server"
          ,"component of the MissingH library.  The MissingH library"
          ,"is available from http://quux.org/devel/missingh."
          ,""
          ,""
          ,"I know of the following commands:"
          ,concatMap (\ (name, (_, (summary, _))) -> vsprintf "%-10s %s\n" name summary)
              (sort commands)
          ,""
          ,"You may type \"HELP command\" for more help on a specific command."
          ]
        in
        if args == ""
           then do sastr <- showSockAddr (remote state)
                   sendReply h 214 (genericreply sastr)
                   return True
           else let newargs = map toUpper args
                    in case lookup newargs commands of
                         Nothing -> do 
                                    sendReply h 214 $ "No help for \"" ++ newargs
                                      ++ "\" is available.\nPlese send HELP"
                                      ++ " without arguments for a list of\n"
                                      ++ "valid commands."
                                    return True
                         Just (_, (summary, detail)) ->
                             do sendReply h 214 $ newargs ++ ": " ++ summary ++ 
                                               "\n\n" ++ detail
                                return True
