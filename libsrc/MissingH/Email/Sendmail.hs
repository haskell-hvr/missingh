{- arch-tag: Sendmail utility
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
   Module     : MissingH.Email.Sendmail
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

This Haskell module provides an interface to transmitting a mail message.

Written by John Goerzen, jgoerzen\@complete.org
-}

module MissingH.Email.Sendmail(sendmail)
where

import MissingH.Cmd
import System.Directory
import System.IO

sendmails = ["/usr/sbin/sendmail",
             "/usr/local/sbin/sendmail",
             "/usr/local/bin/sendmail",
             "/usr/bin/sendmail",
             "/etc/sendmail",
             "/usr/etc/sendmail"]

findsendmail :: IO String
findsendmail =
    let worker [] = return "sendmail"
        worker (this:next) =
            do
            e <- doesFileExist this
            if e then
               do
               p <- getPermissions this
               if executable p then
                  return this
                  else worker next
               else worker next
        in
        worker sendmails

{- | Transmits an e-mail message using the system's mail transport agent.

This function takes a message, a list of recipients, and an optional sender,
and transmits it using the system's MTA, sendmail.

If @sendmail@ is on the @PATH@, it will be used; otherwise, a list of system
default locations will be searched.

A failure will be logged, since this function uses 'MissingH.Cmd.safeSystem'
internally. -}
sendmail :: Maybe String                -- ^ The envelope from address.  If not specified, takes the system's default, which is usually based on the effective userid of the current process.  This is not necessarily what you want, so I recommend specifying it.
         -> [String]                    -- ^ A list of recipients for your message.  An empty list is an error.
         -> String                      -- ^ The message itself.
         -> IO ()
sendmail _ [] _ = fail "sendmail: no recipients specified"
sendmail Nothing recipients msg = sendmail_worker recipients msg
sendmail (Just from) recipients msg = 
    sendmail_worker (("-f" ++ from) : recipients) msg
    
sendmail_worker :: [String] -> String -> IO ()
sendmail_worker args msg =
    let func h = do
                 print "I am the func!"
                 hPutStr h msg
                 print "Func ending!"
        in
        do
        pOpen WriteToPipe "/usr/sbin/sendmail" args func
        {-
        catch (pOpen WriteToPipe "sendmail" args func) 
                  (\_ -> do
                         sn <- findsendmail
                         pOpen WriteToPipe sn args func) -}

