{- arch-tag: ConfigParser types
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
   Module     : MissingH.ConfigParser.Types
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Internal types for "MissingH.ConfigParser".  This module is not intended to be
used directly by your programs.

Copyright (c) 2004 John Goerzen, jgoerzen\@complete.org
-}

module MissingH.ConfigParser.Types (
                                    CPOptions, CPData, 
                                    ConfigParser(..), empty,
                                    fromAL, SectionSpec,
                                    OptionSpec,
                                   ) where
import Data.FiniteMap
import Data.Char
import MissingH.ConfigParser.Parser

{- | Names of sections -}
type SectionSpec = String

{- | Names of options -}
type OptionSpec = String

{- | Storage of options. -}
type CPOptions = FiniteMap OptionSpec String

{- | The main data storage type (storage of sections). -}
type CPData = FiniteMap SectionSpec CPOptions

{- | This is the main record that is used by 'MissingH.ConfigParser'.
-}
data ConfigParser = ConfigParser 
    { -- | The data itself
      content :: CPData,
      -- | How to transform an option into a standard representation
      optionxform :: (OptionSpec -> OptionSpec),
      -- | Function to look up an option, considering a default value
      -- | if 'usedefault' is True; or ignoring a default value otherwise.
      defaulthandler :: (ConfigParser -> SectionSpec -> OptionSpec -> Maybe String),
      -- | Whether or not to seek out a default action when no match
      -- is found.
      usedefault :: Bool,
      -- | Function that is used to perform lookups, do optional
      -- interpolation, etc.
      accessfunc :: (ConfigParser -> SectionSpec -> OptionSpec -> Maybe String)
    }

{- | The default empty 'MissingH.ConfigParser' object.

The content contains only an empty mandatory @DEFAULT@ section.

'optionxform' is set to @map toLower@.

'usedefault' is set to @True@.
-}
empty :: ConfigParser
empty = ConfigParser { content = fromAL [("DEFAULT", [])],
                       optionxform = map toLower,
                       usedefault = True}

{- | Low-level tool to convert a parsed object into a 'CPData'
representation.  Performs no option conversions or special handling
of @DEFAULT@. -}
fromAL :: ParseOutput -> CPData
fromAL origal =
    let conv :: CPData -> (String, [(String, String)]) -> CPData
        conv fm sect = addToFM fm (fst sect) (listToFM $ snd sect)
        in
        foldl conv emptyFM origal
