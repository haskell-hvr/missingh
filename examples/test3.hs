-- example code 3 for socketserver
import MissingH.Network.SocketServer
import MissingH.IO
import MissingH.Logging.Logger
import Data.Char
import System.IO
import MissingH.Str
import System.Time

realhandler h = 
    let loop = do e <- hIsEOF h
                  if e then return ()
                     else do c <- hGetLine h
                             case (rstrip c) of 
                                 "QUIT" -> hPutStr h "Goodbye!\n"
                                 "COMMANDS" -> do hPutStrLn h "You can type TIME for the current time"
                                                  loop
                                 "TIME" -> do ct <- getClockTime
                                              calt <- toCalendarTime ct
                                              hPutStrLn h $ calendarTimeToString calt
                                              loop
                                 x -> do hPutStrLn h (map toUpper x)
                                         loop
        in do hPutStrLn h "Welcome to the uppercase server.  I'll echo"
              hPutStrLn h "everything back to you in uppercase.  When done,"
              hPutStrLn h "just type \"QUIT\" to exit."
              hPutStrLn h "You can also type \"COMMANDS\" for some fun stuff."
              hPutStrLn h ""
              loop
              hClose h

handler = threadedHandler $ loggingHandler "main" INFO $ handleHandler $
            realhandler
main = do updateGlobalLogger "main" (setLevel DEBUG)
          serveTCPforever ((simpleInetOptions 12345) {reuse = True}) handler
