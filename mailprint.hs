-- arch-tag: print and mail doc from stdin
import MissingH.Email.Sendmail
import MissingH.Str
import MissingH.Cmd
import MissingH.Logging.Logger
import MissingH.Logging.Handler.Syslog
import System.IO

recipients = ["jgoerzen@excelhustler.com"]
printer = "isdept"

main = do
       updateGlobalLogger "MissingH.Cmd.safeSystem" (setLevel DEBUG)
       updateGlobalLogger "MissingH.Cmd.pOpen3" (setLevel DEBUG)
       hdlr <- openlog "mailprint" [PID] USER DEBUG
       updateGlobalLogger rootLoggerName (setHandlers [hdlr])
       c <- getContents
       let msg = "From: MFG/Pro System <root@excelhustler.com>\n" ++
             "To: " ++ (join "," recipients) ++ "\n" ++
             "Subject: MFG/Pro Printer Output\n\n" ++ c
       --sendmail Nothing recipients msg
       pOpen WriteToPipe "cat" []
         (\h -> hPutStr h ("FOO: " ++ c ++ ", FOO\n"))

