{- arch-tag: Log handlers main definition
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

{- | Definition of log handlers

Written by John Goerzen, jgoerzen\@complete.org
n-}

module MissingH.Logging.Handler(-- * Basic Types
                                LogHandler(..),
                                -- * Simple Handlers
                                streamHandler, fileHandler
                               ) where
import MissingH.Logging
import IO


{- | This is the base class for the various log handlers.  They should
all adhere to this class. -}

class LogHandler a where
                   -- | Sets the log level.  'handle' will drop
                   -- items beneath this level.
                   setLevel :: a -> Priority -> a
                   -- | Gets the current level.
                   getLevel :: a -> Priority
                   -- | Logs an event if it meets the requirements
                   -- given by the most recent call to 'setLevel'.
                   handle :: a -> LogRecord -> IO ()

                   handle h (pri, msg) = 
                       if pri >= (getLevel h)
                          then emit h (pri, msg)
                          else return ()
                   -- | Forces an event to be logged regardless of
                   -- the configured level.
                   emit :: a -> LogRecord -> IO ()
                   -- | Closes the logging system, causing it to close
                   -- any open files, etc.
                   close :: a -> IO ()


data GenericHandler a = GenericHandler {priority :: Priority,
                                        privData :: a,
                                        writeFunc :: a -> String -> IO (),
                                        closeFunc :: a -> IO () }

instance LogHandler (GenericHandler a) where
    setLevel sh p = sh{priority = p}
    getLevel sh = priority sh
    emit sh lr = (writeFunc sh) (privData sh) (snd lr)
    close sh = (closeFunc sh) (privData sh)

{- | Create a stream log handler.  Log messages sent to this handler will
   be sent to the stream used initially.  Note that the 'close' method
   will have no effect on stream handlers; it does not actually close
   the underlying stream.  -}

streamHandler :: Handle -> Priority -> IO (GenericHandler Handle)
streamHandler h pri = 
    return (GenericHandler {priority = pri,
                            privData = h,
                            writeFunc = hPutStrLn,
                            closeFunc = \x -> return ()})

{- | Create a file log handler.  Log messages sent to this handler
   will be sent to the filename specified, which will be opened
   in Append mode.  Calling close on the handler will close the file.
   -}

fileHandler :: FilePath -> Priority -> IO (GenericHandler Handle)
fileHandler fp pri = do
                     h <- openFile fp AppendMode
                     sh <- streamHandler h pri
                     return (sh{closeFunc = hClose})
