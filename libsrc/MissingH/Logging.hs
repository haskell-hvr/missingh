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

{- |
   Module     : MissingH.Logging
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : experimental
   Portability: portable

Haskell Logging Framework

Written by John Goerzen, jgoerzen\@complete.org

This module defines basic types used for logging.

/This API is new and in a state of flux./

Extensive documentation is available in
"MissingH.Logging.Logger".

-}



module MissingH.Logging(-- * Types
                        Priority(..),
                        LogRecord
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

{- | Internal type of log records -}

type LogRecord = (Priority, String)

