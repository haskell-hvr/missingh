{- arch-tag: Logger main definition
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

{- | Definition of log handler support

For some handlers, check out "MissingH.Logging.Handler.Simple" and
"MissingH.Logging.Handler.Syslog".

Written by John Goerzen, jgoerzen\@complete.org
-}

module MissingH.Logging.Logger(-- * Basic Types
                               Logger(..)
                               ) where
import MissingH.Logging
import MissingH.Logging.Handler
import MissingH.Logging.Handler.Simple
import IO
import System.IO.Unsafe
import Data.IORef
import Data.List(map)

data HandlerT = forall a. LogHandler a => HandlerT a

data Logger = Logger { priority :: Priority,
                       handlers :: [HandlerT]}

rootLogger :: IORef Logger
rootLogger = unsafePerformIO $ do
                               h <- streamHandler stdout DEBUG
                               newIORef (Logger 
                                         {priority = NOTICE,
                                          handlers = [HandlerT h]})

callHandler :: Priority -> String -> HandlerT -> IO ()
callHandler pri msg ht =
    case ht of
            HandlerT x -> handle x (pri, msg)

handlerActions :: Priority -> String -> IO [IO ()]
handlerActions pri msg = do
                         l <- readIORef rootLogger
                         let h = map (callHandler pri msg) (handlers l)
                         return h
                         
log :: Priority -> String -> IO ()
log pri msg = do
              l <- readIORef rootLogger
              a <- handlerActions pri msg
              if (pri >= priority l) 
                 then sequence_ a
                 else return ()
