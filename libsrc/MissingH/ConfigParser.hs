{- arch-tag: ConfigParser main file
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
   Module     : MissingH.ConfigParser
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Configuration file parsing, generation, and manipulation

Copyright (c) 2004 John Goerzen, jgoerzen\@complete.org
-}
module MissingH.ConfigParser
    (
     -- * Initialization
     empty,
     -- * Reading
     -- * Whole-File Manipulation
     merge
) where
import MissingH.ConfigParser.Types
import MissingH.ConfigParser.Parser
import Data.FiniteMap

{- | Combines two 'ConfigParser's into one.

Any duplicate options are resolved to contain the value specified in
the second parser.

The 'ConfigParser' options in the resulting object will be set as they
are in the second one passed to this function. -}
merge :: ConfigParser -> ConfigParser -> ConfigParser
merge src dest = 
    let conv :: String -> String
        conv = optionxform dest
        convFM :: String -> CPOptions -> CPOptions
        convFM _ = listToFM . map (\x -> (conv (fst x), snd x)) . fmToList
        in
        ConfigParser { content = plusFM (mapFM convFM (content src)) 
                                 (content dest),
                       optionxform = optionxform dest,
                       usedefault = usedefault dest }
