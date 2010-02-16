-- example code 2 for socketserver
import MissingH.Network.SocketServer
import MissingH.IO
import MissingH.Logging.Logger
import Data.Char
import System.IO
import MissingH.Str

lineInteraction :: [String] -> [String]
lineInteraction inp =
   let realInteract :: [String] -> [String]
       realInteract [] = []
       realInteract ("QUIT":_) = ["Goodbye!"]
       realInteract ("easeregg":_) = ["Yow!"]
       realInteract (x:xs) = map toUpper x : realInteract xs
       in
       ("Welcome to the uppercase server.  I'll echo everything back to\n" ++
         "you in uppercase.  When done, just type \"QUIT\" to exit.\n") :
         realInteract (map rstrip inp)

realhandler h = do hLineInteract h h lineInteraction
                   hClose h
handler = threadedHandler $ loggingHandler "main" INFO $ handleHandler $
            realhandler
main = do updateGlobalLogger "main" (setLevel DEBUG)
          serveTCPforever ((simpleInetOptions 12345) {reuse = True}) handler
