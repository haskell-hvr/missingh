{- arch-tag: Command utilities main file
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
   Module     : MissingH.Cmd
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable to platforms with rawSystem

 Command invocation utilities.

Written by John Goerzen, jgoerzen\@complete.org
-}

module MissingH.Cmd(safeSystem)
where

import System.Exit
import System.Cmd
import MissingH.Logging.Logger

logbase = "MissingH.Cmd"

{- | Invokes the specified command in a subprocess, waiting for the result.
If the command terminated successfully, return normally.  Otherwise,
raises a userError with the problem.

Command lines executed will be logged using "MissingH.Logging.Logger" at the
DEBUG level.  Failure messages will be logged at the WARNING level in addition
to being raised as an exception.  Both are logged under
\"MissingH.Cmd.safeSystem\".  If you wish to suppress these messages
globally, you can simply run:

> updateGlobalLogger "MissingH.Cmd.safeSystem"
>                     (setLevel CRITICAL)

See also: 'MissingH.Logging.Logger.updateGlobalLogger',
"MissingH.Logging.Logger".

-}

safeSystem :: FilePath -> [String] -> IO ()
safeSystem command args = 
    do
    debugM (logbase ++ ".safeSystem")
               ("Running: " ++ command ++ " " ++ (show args))
    ec <- rawSystem command args
    case ec of
            ExitSuccess -> return ()
            ExitFailure fc -> cmdfailed command args fc

cmdfailed :: FilePath -> [String] -> Int -> IO ()
cmdfailed command args failcode = do
    let errormsg = "Command " ++ command ++ " " ++ (show args) ++
            " failed; exit code " ++ (show failcode)
    let e = userError (errormsg)
    warningM (logbase ++ ".safeSystem") errormsg
    ioError e


