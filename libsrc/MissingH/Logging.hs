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

{- | Basic logging types

Written by John Goerzen, jgoerzen\@complete.org

This module defines basic types used for logging.

-}

module MissingH.Logging(Priority(..),
                        LogRecord)
    where

{- | Priorities are used to define how important a log messgae is.
Users can filter log messages based on priorities.

These have their roots on the traditional syslog system.  The standard
definitions are given below, but you are free to interpret them however you
like.  They are listed here in descending importance order.
-}

data Priority = EMERG                   -- ^ System is unusable
              | ALERT                   -- ^ Take immediate action
              | CRITICAL                -- ^ Severe situations
              | ERROR                   -- ^ General Errors
              | WARNING                 -- ^ General Warnings
              | NOTICE                  -- ^ Normal runtime conditions
              | INFO                    -- ^ Information
              | DEBUG                   -- ^ Debug messages

type LogRecord = (Priority, String)

