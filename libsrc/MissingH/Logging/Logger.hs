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
                               -- * Finding Loggers
                               getLogger, getRootLogger,
                               -- * Logging Messages
                               logL,
                               -- * Modifying Loggers
                               addHandler, getLevel, setLevel,
                               updateGlobalLogger

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


-- | Add handler to 'Logger'.  Returns a new 'Logger'.
addHandler :: LogHandler a => Logger -> a -> Logger
addHandler l h = l{handlers = (HandlerT h) : (handlers l)}

-- | Returns the "level" of the logger.  Items beneath this
-- level will be ignored.

getLevel :: Logger -> Priority
getLevel l = level l

-- | Sets the "level" of the 'Logger'.  Returns a new
-- 'Logger' object with the new level.

setLevel :: Logger -> Priority -> Logger
setLevel l p = l{level = p}

-- | Updates the global record for the given logger to take into
-- account any changes you may have made.

updateGlobalLogger :: Logger -> IO ()
updateGlobalLogger l = modifyIORef logTree (\a -> addToFM a (name l) l)
