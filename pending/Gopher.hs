{- arch-tag: Gopher support
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
   Module     : MissingH.Network.Gopher
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : experimental
   Portability: systems with networking

This module provides types and generic support for Gopher clients or serves.

Related standards: RFC1436 <http://www.faqs.org/rfcs/rfc1436.html>,
Gopher+ spec <gopher://gopher.quux.org/1/Archives/mirrors/boombox.micro.umn.edu/pub/gopher/gopher_protocol/Gopher%2B/Gopher%2B.txt>

Written by John Goerzen, jgoerzen\@complete.org
-}

module MissingH.Network.Gopher (-- * Types
                                GopherEntry(..)
                               )
    where

import MissingH.Printf
import MissingH.Str
import Data.FiniteMap

{- | Type representing an entry in a Gopher directory.

May add more Gopher+ stuff in here down the road.

You can show a 'GopherEntry'.  This will produce a one-line string suitable
for use on a Gopher server.

You can 'read' to a 'GopherEntry'.  This will parse a string as a one-line
piece of text suitable for use generating a 'GopherEntry'.

Neither show nor read will consider the 'ea' member. -}
data GopherEntry = GopherEntry 
    {
     selector :: String,                -- ^ Path to file on server
     gophertype :: Char,                -- ^ Gopher0 type character
     name :: String,                    -- ^ Gopher menu name
     host :: String,                    -- ^ Content host name
     port :: Integer,                   -- ^ Remote port
     gopherpsupport :: Bool,            -- ^ Whether Gopher+ is supported
     ea :: FiniteMap String String      -- ^ Gopher+ extended attributes
    }

instance Show GopherEntry where
    show x = let basic = vsprintf "%c%s\t%s\t%s\t%d"
                             (gophertype x) (name x) (selector x) (host x)
                                            (port x)
                 in if gopherpsupport x then
                    basic ++ "\t+"
                    else basic

instance Read GopherEntry where
    read s = let parts = split "\t" s
                 in
                 GopherEntry {selector = parts !! 2,
                              gophertype = head (parts !! 0),
                              name = parts !! 1,
                              host = parts !! 3,
                              port = read (parts !! 4),
                              gopherpsupport = length parts > 5 && (parts !! 5 == "+"),
                              ea = emptyFM
                             }
