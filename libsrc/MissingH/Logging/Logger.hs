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
                               Logger,
                               componentsOfName

                               ) where
import MissingH.Str
import MissingH.Logging
import MissingH.Logging.Handler(LogHandler)
import qualified MissingH.Logging.Handler(handle)
import MissingH.Logging.Handler.Simple
import IO
import System.IO.Unsafe
import Data.IORef
import Data.List(map)
import Data.FiniteMap

-----------------------------------------------------------------------
-- Basic types

data HandlerT = forall a. LogHandler a => HandlerT a

data Logger = Logger { level :: Priority,
                       handlers :: [HandlerT],
                       name :: String}

type LogTree = FiniteMap String Logger

-----------------------------------------------------------------------
-- Module shortcuts

rootlogger = ""

-----------------------------------------------------------------------
-- Utilities

-- | Placeholders created when a new logger must be created.
placeholder :: Logger
placeholder = Logger {level = DEBUG, handlers = [], name = ""}

-----------------------------------------------------------------------
-- Logger Tree Storage

-- | The log tree.  Initialize it with a default root logger 
-- and (FIXME) a logger for MissingH itself.

{-# NOINLINE logTree #-}

logTree :: IORef LogTree
-- note: only kick up tree if handled locally
logTree = 
    unsafePerformIO $ do
                      h <- streamHandler stderr DEBUG
                      newIORef (unitFM rootlogger (Logger 
                                                   {level = WARNING,
                                                    name = "",
                                                    handlers = [HandlerT h]}))

-- | Given a name, return all components of it, starting from the root.
componentsOfName :: String -> [String]
componentsOfName name =
    let joinComp [] _ = []
        joinComp (x:xs) [] = x : joinComp xs x
        joinComp (x:xs) accum =
            let newlevel = accum ++ "." ++ x in
                newlevel : joinComp xs newlevel
        in
        rootlogger : joinComp (split "." name) []

createLoggers :: [String] -> IO ()
createLoggers [] = return ()
createLoggers (x:xs) = 
    do
    lt <- readIORef logTree
    if elemFM x lt
       then createLoggers xs
       else do
         modifyIORef logTree (\a -> addToFM a x (placeholder {name = x}))
         createLoggers xs


-- | Returns the logger for the given name.  If no logger with that name
-- exists, creates new loggers and any necessary parent loggers, with
-- no connected handlers.

getLogger :: String -> IO Logger
getLogger lname =
    do
    lt <- readIORef logTree
    case lookupFM lt lname of
         Just x -> return x
         Nothing -> do
                    -- Add it.  Then call myself to retrieve it.
                    createLoggers (componentsOfName lname)
                    getLogger lname
                            
-- | Returns the root logger.

getRootLogger :: IO Logger
getRootLogger = getLogger ""

-- | Log a message, assuming the current logger's
log :: Logger -> Priority -> String -> IO ()
log l pri msg = handle l (pri, msg)

-- | Handle a log request.
handle :: Logger -> LogRecord -> IO ()
handle l (pri, msg) = 
    if pri >= (level l)
       then do 
            sequence_ (handlerActions (handlers l) (pri, msg))
            -- Send it upstairs if we can
            case (name l) of
                "" -> return ()
                x -> do 
                     parent <- (getLogger . head . reverse . componentsOfName) x
                     handle parent (pri, msg)
       else return ()


-- | Call a handler given a HandlerT.
callHandler :: LogRecord -> HandlerT -> IO ()
callHandler lr ht =
    case ht of
            HandlerT x -> MissingH.Logging.Handler.handle x lr

-- | Generate IO actions for the handlers.
handlerActions :: [HandlerT] -> LogRecord -> [IO ()]
handlerActions h lr = map (callHandler lr) h
                         
-- | Add handler to logger.
addHandler :: LogHandler a => Logger -> a -> Logger
addHandler l h = l{handlers = (HandlerT h) : (handlers l)}

