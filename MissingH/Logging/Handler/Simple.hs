{- arch-tag: Simple log handlers
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
   Module     : MissingH.Logging.Handler.Simple
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Simple log handlers

Written by John Goerzen, jgoerzen\@complete.org
-}

module MissingH.Logging.Handler.Simple(streamHandler, fileHandler)
    where

import MissingH.Logging
import MissingH.Logging.Handler
import IO


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
   in Append mode.  Calling 'close' on the handler will close the file.
   -}

fileHandler :: FilePath -> Priority -> IO (GenericHandler Handle)
fileHandler fp pri = do
                     h <- openFile fp AppendMode
                     sh <- streamHandler h pri
                     return (sh{closeFunc = hClose})
