{- arch-tag: Logging Main Definition
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

{- | Logging Infrastructure for Haskell

Written by John Goerzen, jgoerzen\@complete.org

This module defines basic types used for logging.

/This API is new and in a state of flux./

-}



module MissingH.Logging(-- * Types
                        Priority(..),
                        LogRecord,
                        Logger,
                        -- * Logging Messages
                        -- ** Basic
                        logM,
                        -- ** Utility Functions
                        -- These functions are wrappers for 'logM' to help
                        -- make your job easier.
                        debugM, infoM, noticeM, warningM, errorM, criticalM,
                        alertM, emergencyM,
                        -- * Logger Manipulation
                        -- More functions are available in
                        -- "MissingH.Logging.Logger".
                        -- ** Finding \/ Creating Loggers
                        getLogger, getRootLogger, rootLoggerName,
                        -- ** Logging to a particular Logger
                        logL
)

    where

{- | Priorities are used to define how important a log messgae is.
Users can filter log messages based on priorities.

These have their roots on the traditional syslog system.  The standard
definitions are given below, but you are free to interpret them however you
like.  They are listed here in ascending importance order.
-}

data Priority = 
            DEBUG                   -- ^ Debug messages
          | INFO                    -- ^ Information
          | NOTICE                  -- ^ Normal runtime conditions
          | WARNING                 -- ^ General Warnings
          | ERROR                   -- ^ General Errors
          | CRITICAL                -- ^ Severe situations
          | ALERT                   -- ^ Take immediate action
          | EMERGENCY               -- ^ System is unusable
                    deriving (Eq, Ord, Show, Read)

type LogRecord = (Priority, String)

---------------------------------------------------------------------------
-- Basic logger types
---------------------------------------------------------------------------
data HandlerT = forall a. LogHandler a => HandlerT a

data Logger = Logger { level :: Priority,
                       handlers :: [HandlerT],
                       name :: String}


type LogTree = FiniteMap String Logger

{- | This is the base class for the various log handlers.  They should
all adhere to this class. -}


---------------------------------------------------------------------------
-- Utilities
---------------------------------------------------------------------------

-- | The name of the root logger, which is always defined and present
-- on the system.
rootLoggerName = ""

-- | Placeholders created when a new logger must be created.
placeholder :: Logger
placeholder = Logger {level = DEBUG, handlers = [], name = ""}

---------------------------------------------------------------------------
-- Logger Tree Storage
---------------------------------------------------------------------------

-- | The log tree.  Initialize it with a default root logger 
-- and (FIXME) a logger for MissingH itself.

{-# NOINLINE logTree #-}

logTree :: IORef LogTree
-- note: only kick up tree if handled locally
logTree = 
    unsafePerformIO $ do
                      h <- streamHandler stderr DEBUG
                      newIORef (unitFM rootLoggerName (Logger 
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
        rootLoggerName : joinComp (split "." name) []

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

---------------------------------------------------------------------------
-- Logging With Location
---------------------------------------------------------------------------

{- | Log a message using the given logger at a given priority. -}

logM :: String                           -- ^ Name of the logger to use
     -> Priority                         -- ^ Priority of this message
     -> String                           -- ^ The log text itself
     -> IO ()

logM logname pri msg = do
                       l <- getLogger logname
                       logL l pri msg

---------------------------------------------------------------------------
-- Utility functions
---------------------------------------------------------------------------

{- | Log a message at 'DEBUG' priority -}
debugM :: String                         -- ^ Logger name
      -> String                         -- ^ Log message
      -> IO ()
debugM s = logM s DEBUG

{- | Log a message at 'INFO' priority -}
infoM :: String                         -- ^ Logger name
      -> String                         -- ^ Log message
      -> IO ()
infoM s = infoM s INFO

{- | Log a message at 'NOTICE' priority -}
noticeM :: String                         -- ^ Logger name
      -> String                         -- ^ Log message
      -> IO ()
noticeM s = logM s NOTICE

{- | Log a message at 'WARNING' priority -}
warningM :: String                         -- ^ Logger name
      -> String                         -- ^ Log message
      -> IO ()
warningM s = logM s WARNING

{- | Log a message at 'ERROR' priority -}
errorM :: String                         -- ^ Logger name
      -> String                         -- ^ Log message
      -> IO ()
errorM s = logM s ERROR

{- | Log a message at 'CRITICAL' priority -}
criticalM :: String                         -- ^ Logger name
      -> String                         -- ^ Log message
      -> IO ()
criticalM s = logM s CRITICAL

{- | Log a message at 'ALERT' priority -}
alertM :: String                         -- ^ Logger name
      -> String                         -- ^ Log message
      -> IO ()
alertM s = logM s ALERT

{- | Log a message at 'EMERGENCY' priority -}
emergencyM :: String                         -- ^ Logger name
      -> String                         -- ^ Log message
      -> IO ()
emergencyM s = logM s EMERGENCY

---------------------------------------------------------------------------
-- Public Logger Interaction Support
---------------------------------------------------------------------------

-- | Returns the logger for the given name.  If no logger with that name
-- exists, creates new loggers and any necessary parent loggers, with
-- no connected handlers.

getLogger :: String -> IO Logger
getLogger lname =
    do
    --putStrLn lname
    lt <- readIORef logTree
    --putStrLn (show (keysFM lt))
    case lookupFM lt lname of
         Just x ->  return x
         Nothing -> do
                    --print "Missed it."
                    -- Add it.  Then call myself to retrieve it.
                    createLoggers (componentsOfName lname)
                    --putStrLn "createLoggers done"
                    getLogger lname

                            
-- | Returns the root logger.

getRootLogger :: IO Logger
getRootLogger = getLogger rootLoggerName

-- | Log a message, assuming the current logger's level permits it.
logL :: Logger -> Priority -> String -> IO ()
logL l pri msg = handle l (pri, msg)

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
                     parent <- (getLogger . head . drop 1 . reverse . componentsOfName) x
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
                         
