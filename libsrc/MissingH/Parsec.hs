{- arch-tag: Parsec utilities
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
   Module     : MissingH.Parsec
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Written by John Goerzen, jgoerzen\@complete.org

-}

module MissingH.Parsec(notMatching)
where

import Text.ParserCombinators.Parsec

{- | Running @notMatching p msg@ will try to apply parser p.
If it fails, returns ().  If it succeds, cause a failure and raise
the given error message.  It will not consume input in either case. -}
notMatching :: GenParser a b c -> String -> GenParser a b ()
notMatching p errormsg = 
    let maybeRead = try (do 
                         x <- p
                         return (Just x)
                        )
                    <|> return Nothing
        workerFunc =  do
                      x <- maybeRead
                      case x of
                             Nothing -> return ()
                             Just x -> unexpected errormsg
        in
        try workerFunc

