module MissingH.Wash.Mail.EmailConfig where

tmpDir, varDir, emailTmpDir, sendmailProgram :: String

-- |temporary storage
tmpDir = "/tmp/"
-- |persistent, mutable storage
varDir = "/tmp/"

-- |temporary email files
emailTmpDir 	= tmpDir
-- |path of sendmail program
sendmailProgram	= "/usr/sbin/sendmail"
