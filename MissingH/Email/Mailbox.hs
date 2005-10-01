{-
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

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
   Module     : MissingH.Email.Mailbox
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen\@complete.org
   Stability  : provisional
   Portability: portable

General support for e-mail mailboxes

Written by John Goerzen, jgoerzen\@complete.org
-}

module MissingH.Email.Mailbox(Flag(..), Message, Flags,
                              MailboxReader(..),
                              MailboxWriter(..))
where

type Message = String

data Flag = 
           SEEN
           | ANSWERED
           | FLAGGED
           | DELETED
           | DRAFT
           | OTHERFLAG String
           deriving (Eq, Show)
           
type Flags = [Flag]

class (Show a, Show b) => MailboxReader a b where
    listMessageIDs :: a -> IO [b]
    listMessageFlags :: a -> IO [(b, Flags)]
    getAll :: a -> IO [(b, Flags, Message)]
    getMessages :: a -> [b] -> IO [(b, Flags, Message)]
    
class (MailboxReader a b) => MailboxWriter a b where
    appendMessage :: a -> Flags -> Message -> IO b
    deleteMessages :: a -> [b] -> IO ()
    addFlags :: a -> [b] -> Flags -> IO ()
    removeFlags :: a -> [b] -> Flags -> IO ()
    setFlags :: a -> [b] -> Flags -> IO ()

