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
as defined by RFC959 and RFC1123.

Written by John Goerzen, jgoerzen\@complete.org

-}

module MissingH.Network.FTP.Server(
                                   anonFtpHandler
                                  )
where
import MissingH.Network.FTP.ParserServer
import Network.BSD
import Network.Socket
import qualified Network
import System.IO
import MissingH.Logging.Logger
import MissingH.Network
import MissingH.Str
import MissingH.Printf
import MissingH.IO.HVIO
import MissingH.IO.HVFS
import Data.Char
import MissingH.Printf
import Data.IORef
import Data.List

data FTPState = NoAuth 
              | User String
              | Authenticated String
data FTPServer = forall a. HVFS a => FTPServer Handle a (IORef FTPState)

s_crlf = "\r\n"
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

anonFtpHandler :: forall a. HVFS a => a -> Handle -> SockAddr -> IO ()
anonFtpHandler f h sa =
    let serv r = FTPServer h f r
        in
        traplogging "MissingH.Network.FTP.Server" NOTICE "" $
          do r <- newIORef (NoAuth)
             let s = serv r
             sendReply s 220 "Welcome to MissingH.Network.FTP.Server."
             commandLoop s sa

type CommandHandler = FTPServer -> SockAddr -> String -> IO Bool
type Command = (String, (CommandHandler, (String, String)))

instance Eq Command where
    x == y = (fst x) == (fst y)
instance Ord Command where
    compare x y = compare (fst x) (fst y)

commands :: [Command]
commands =
    [("HELP", (cmd_help, help_help))
    ,("QUIT", (cmd_quit, help_quit))
    ]

commandLoop :: FTPServer -> SockAddr -> IO ()
commandLoop h@(FTPServer fh _ _) sa =
    let errorhandler e = do noticeM "MissingH.Network.FTP.Server"
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
                            Nothing -> do sendReply h 500 $
                                           "Unrecognized command " ++ cmd
                                          return True
                            Just hdlr -> (fst hdlr) h sa args
               )
              if continue
                 then commandLoop h sa
                 else return ()

help_quit =
    ("Terminate the session",
     "")

cmd_quit :: CommandHandler
cmd_quit h sa args =
    do sendReply h 211 "OK, Goodbye."
       return False

help_help =
    ("Display help on available commands",
     "When called without arguments, shows a summary of available system\n"
     ++ "commands.  When called with an argument, shows detailed information\n"
     ++ "on that specific command.")

cmd_help :: CommandHandler
cmd_help h sa args =
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
           then do sastr <- showSockAddr sa
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
