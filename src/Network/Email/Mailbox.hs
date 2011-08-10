{-
Copyright (c) 2005-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : Network.Email.Mailbox
   Copyright  : Copyright (C) 2005-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable

General support for e-mail mailboxes

Written by John Goerzen, jgoerzen\@complete.org
-}

module Network.Email.Mailbox(Flag(..), Flags, Message,
                              MailboxReader(..),
                              MailboxWriter(..))
where

{- | The flags which may be assigned to a message. -}
data Flag = 
           SEEN
           | ANSWERED
           | FLAGGED
           | DELETED
           | DRAFT
           | FORWARDED
           | OTHERFLAG String
           deriving (Eq, Show)
           
{- | Convenience shortcut -}
type Flags = [Flag]

{- | A Message is represented as a simple String. -}
type Message = String

{- | Main class for readable mailboxes. 

The mailbox object /a/ represents zero or more 'Message's.  Each message
has a unique identifier /b/ in a format specific to each given mailbox.
This identifier may or may not be persistent.

Functions which return a list are encouraged -- but not guaranteed -- to
do so lazily.

Implementing classes must provide, at minimum, 'getAll'.
-}
class (Show a, Show b, Eq b) => MailboxReader a b where
    {- | Returns a list of all unique identifiers. -}
    listIDs :: a -> IO [b]
    {- | Returns a list of all unique identifiers as well as all flags. -}
    listMessageFlags :: a -> IO [(b, Flags)]
    {- | Returns a list of all messages, including their content,
       flags, and unique identifiers. -}
    getAll :: a -> IO [(b, Flags, Message)]
    {- | Returns information about specific messages. -}
    getMessages :: a -> [b] -> IO [(b, Flags, Message)]

    listIDs mb = listMessageFlags mb >>= return . map fst
    listMessageFlags mb = getAll mb >>= return . 
                           map (\(i, f, _) -> (i, f))
    getMessages mb list =
        do messages <- getAll mb
           return $ filter (\(id, f, m) -> id `elem` list) messages
    
class (MailboxReader a b) => MailboxWriter a b where
    appendMessages :: a -> [(Flags, Message)] -> IO [b]
    deleteMessages :: a -> [b] -> IO ()
    addFlags :: a -> [b] -> Flags -> IO ()
    removeFlags :: a -> [b] -> Flags -> IO ()
    setFlags :: a -> [b] -> Flags -> IO ()
