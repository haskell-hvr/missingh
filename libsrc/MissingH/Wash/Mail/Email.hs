-- © 2001, 2002 Peter Thiemann
module MissingH.Wash.Mail.Email (
	sendmail, inventMessageId, exitcodeToSYSEXIT, SYSEXIT(..),
	module MissingH.Wash.Mail.MIME, 
        module MissingH.Wash.Mail.HeaderField) where

-- from standard library
import IO
import System

-- from utility
import MissingH.Wash.Utility.Auxiliary
import MissingH.Wash.Utility.Unique

-- from package
import MissingH.Wash.Mail.EmailConfig
import MissingH.Wash.Mail.HeaderField
import MissingH.Wash.Mail.MIME

-- |from sysexit.h
data SYSEXIT =
	EX_OK		--		0	/* successful termination */
      | EX_USAGE	--		64	/* command line usage error */
      | EX_DATAERR	--		65	/* data format error */
      | EX_NOINPUT	--		66	/* cannot open input */
      | EX_NOUSER	--		67	/* addressee unknown */
      | EX_NOHOST	--		68	/* host name unknown */
      | EX_UNAVAILABLE	--		69	/* service unavailable */
      | EX_SOFTWARE	--		70	/* internal software error */
      | EX_OSERR	--		71	/* system error (e.g., can't fork) */
      | EX_OSFILE	--		72	/* critical OS file missing */
      | EX_CANTCREAT	--		73	/* can't create (user) output file */
      | EX_IOERR	--		74	/* input/output error */
      | EX_TEMPFAIL	--		75	/* temp failure; user is invited to retry */
      | EX_PROTOCOL	--		76	/* remote error in protocol */
      | EX_NOPERM	--		77	/* permission denied */
      | EX_CONFIG	--		78	/* configuration error */
      | EX_UNKNOWN Int

exitcodeToSYSEXIT :: ExitCode -> SYSEXIT
exitcodeToSYSEXIT exitcode =
  case exitcode of
    ExitSuccess -> EX_OK
    ExitFailure 64 -> EX_USAGE
    ExitFailure 65 -> EX_DATAERR
    ExitFailure 66 -> EX_NOINPUT
    ExitFailure 67 -> EX_NOUSER
    ExitFailure 68 -> EX_NOHOST
    ExitFailure 69 -> EX_UNAVAILABLE
    ExitFailure 70 -> EX_SOFTWARE
    ExitFailure 71 -> EX_OSERR
    ExitFailure 72 -> EX_OSFILE
    ExitFailure 73 -> EX_CANTCREAT
    ExitFailure 74 -> EX_IOERR
    ExitFailure 75 -> EX_TEMPFAIL
    ExitFailure 76 -> EX_PROTOCOL
    ExitFailure 77 -> EX_NOPERM
    ExitFailure 78 -> EX_CONFIG
    ExitFailure sc -> EX_UNKNOWN sc

instance Show SYSEXIT where
  showsPrec i se = case se of
      EX_OK	-> showString "successful termination"
      EX_USAGE	-> showString "command line usage error"
      EX_DATAERR	-> showString "data format error"
      EX_NOINPUT	-> showString "cannot open input"
      EX_NOUSER	-> showString "addressee unknown"
      EX_NOHOST	-> showString "host name unknown"
      EX_UNAVAILABLE	-> showString "service unavailable"
      EX_SOFTWARE	-> showString "internal software error"
      EX_OSERR	-> showString "system error (e.g., can't fork)"
      EX_OSFILE	-> showString "critical OS file missing"
      EX_CANTCREAT	-> showString "can't create (user) output file"
      EX_IOERR	-> showString "input/output error"
      EX_TEMPFAIL	-> showString "temp failure; user is invited to retry"
      EX_PROTOCOL	-> showString "remote error in protocol"
      EX_NOPERM	-> showString "permission denied"
      EX_CONFIG	-> showString "configuration error"
      EX_UNKNOWN sc -> showString "unknown return code: " . shows sc

-- facilities for sending email

sendmailFlags   = 
	["-i"			    -- ignore dots alone on a line
	,"-t"			    -- read message for recipients
	,"--"                       -- end of flag arguments
	]			    -- , "-v" for verbose mode

sendmail :: Mail -> IO ExitCode
sendmail mail =
  do filename <- inventBoundary
     let tempfilename  = emailTmpDir ++ filename
         tempfilename2 = emailTmpDir ++ "T" ++ filename
     h <- openFile tempfilename WriteMode
     hSend smtpSendControl{ sendH = h } mail
     hClose h
     exitcode <- system (sendmailProgram ++ pFlags sendmailFlags ++ " < " ++ tempfilename ++ " > " ++ tempfilename2)
     system ("rm " ++ tempfilename)
     system ("rm " ++ tempfilename2)
     return exitcode

pFlags [] = ""
pFlags (flag:flags) = ' ' : flag ++ pFlags flags

inventMessageId :: IO Header
inventMessageId =
  do randomKey <- inventStdKey
     hostname  <- protectedGetEnv "SERVER_NAME" "localhost"
     let messageId = "<" ++ randomKey ++ ".Email@" ++ hostname ++ ">"
     return (Header ("Message-Id", messageId))

