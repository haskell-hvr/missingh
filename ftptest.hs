-- arch-tag: FTP test
import MissingH.Network.FTP.Server
import MissingH.Network.SocketServer
import MissingH.Logging.Logger
import MissingH.IO.HVFS
import MissingH.IO.HVFS.Combinators

main = do
       updateGlobalLogger "" (setLevel DEBUG)
       updateGlobalLogger "MissingH.Network.FTP.Server" (setLevel DEBUG)
       let opts = (simpleTCPOptions 12345) {reuse = True}
       serveTCPforever opts $
            threadedHandler $ 
            loggingHandler "" INFO $
            handleHandler $
            anonFtpHandler (HVFSReadOnly SystemFS)
