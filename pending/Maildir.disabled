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
   Module     : MissingH.Email.Mailbox.Maildir
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable

Support for Maildir-style mailboxes.

Information about the Maildir format can be found at:

 * <http://www.qmail.org/qmail-manual-html/man5/maildir.html>

 * <http://cr.yp.to/proto/maildir.html>

Written by John Goerzen, jgoerzen\@complete.org
-}

module MissingH.Email.Mailbox.Maildir(Maildir(..), readMaildir)
where

import MissingH.Email.Mailbox
import System.Posix.IO(OpenMode(..))
import System.Directory
import MissingH.Path
import MissingH.Maybe
import Control.Monad
import Text.Regex

data Maildir = Maildir 
    {loc :: FilePath}
instance Show Maildir
    where show x = loc x
    
{-
splitFN :: String -> (String, Flags)
                   
splitFN fn = 
    
    where (base, fstr) = 
     case span (/= ':') of
              (h, []) = (h, [])
              (h, f) = (h, tail f)

-}
{- | Open a Maildir mailbox. -}
-- For reading only, for now.

openMaildir :: FilePath -> IO Maildir
openMaildir fp =
    do cwd <- getCurrentDirectory
       let abspath = forceMaybeMsg "abspath readMaildir" $ absNormPath cwd fp
       c <- getDirectoryContents fp
       unless ("cur" `elem` c && "new" `elem` c && "tmp" `elem` c)
              $ error (fp ++ " is not a valid Maildir.")
       return (Maildir fp)
