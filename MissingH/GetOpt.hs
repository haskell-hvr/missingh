{- 
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

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
   Module     : MissingH.getOpt
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen,
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Written by John Goerzen, jgoerzen\@complete.org

Utilities for command-line parsing, including wrappers around
the standard System.Console.GetOpt module.
-}
module MissingH.GetOpt (
                       )
where
import System.Console.GetOpt
import System.Environment

parseCmdLine :: ArgOrder a -> [OptDescr a] -> String -> IO ([a], [String])
parseCmdLine order options header = 
    do argv <- getArgs
       case getOpt order options argv of
         (o, n, []) -> return (o, n)
         (_, _, errors) -> ioError (userError (concat errors ++ 
                                               usageInfo header options))
