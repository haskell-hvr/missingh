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

module MissingH.Cmd(PipeMode(..),
                    safeSystem,
                   pOpen, pOpen3)
where

import System.Exit
import System.Cmd
import MissingH.Logging.Logger
import System.Posix.IO
import System.Posix.Process
import System.Posix.Types
import System.IO
import qualified System.Posix.Signals

data PipeMode = ReadFromPipe | WriteToPipe

logbase = "MissingH.Cmd"

{- | Invokes the specified command in a subprocess, waiting for the result.
If the command terminated successfully, return normally.  Otherwise,
raises a userError with the problem.

Command lines executed will be logged using "MissingH.Logging.Logger" at the
DEBUG level.  Failure messages will be logged at the WARNING level in addition
to being raised as an exception.  Both are logged under
\"MissingH.Cmd.funcname\" -- for instance,
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
            ExitFailure fc -> cmdfailed "safeSystem" command args fc

cmdfailed :: String -> FilePath -> [String] -> Int -> IO a
cmdfailed funcname command args failcode = do
    let errormsg = "Command " ++ command ++ " " ++ (show args) ++
            " failed; exit code " ++ (show failcode)
    let e = userError (errormsg)
    warningM (logbase ++ "." ++ funcname) errormsg
    ioError e

{- | Open a pipe to the specified command.

Passes the handle on to the specified function.

The 'PipeMode' specifies what you will be doing.  That is, specifing 'ReadFromPipe' 
sets up a pipe from stdin, and 'WriteToPipe' sets up a pipe from stdout.

 -}
pOpen :: PipeMode -> FilePath -> [String] -> 
         (Handle -> IO a) -> IO a
pOpen pm fp args func =
        do
        pipepair <- createPipe
        case pm of
         ReadFromPipe -> do
                         let callfunc _ = do
                                        closeFd (snd pipepair)
                                        h <- fdToHandle (fst pipepair)
                                        x <- func h
                                        hClose h
                                        return $! x
                         pOpen3 Nothing (Just (snd pipepair)) Nothing fp args
                                callfunc
         WriteToPipe -> do 
                        let callfunc _ = do
                                       closeFd (fst pipepair)
                                       h <- fdToHandle (snd pipepair)
                                       x <- func h
                                       hClose h
                                       return $! x
                        pOpen3 (Just (fst pipepair)) Nothing Nothing fp args
                               callfunc

{- | Runs a command, redirecting things to pipes. -}
pOpen3 :: Maybe Fd                      -- ^ Send stdin to this fd
       -> Maybe Fd                      -- ^ Get stdout from this fd
       -> Maybe Fd                      -- ^ Get stderr from this fd
       -> FilePath                      -- ^ Command to run
       -> [String]                      -- ^ Command args
       -> (ProcessID -> IO a)           -- ^ Action to run
       -> IO a
pOpen3 pin pout perr fp args func = 
    let mayberedir Nothing _ = return ()
        mayberedir (Just fromfd) tofd = do
                                        dupTo fromfd tofd
                                        return ()
        childstuff = do
                     mayberedir pin stdInput
                     mayberedir pout stdOutput
                     mayberedir perr stdError
                     debugM (logbase ++ ".pOpen3")
                            ("Running: " ++ fp ++ " " ++ (show args))
                     catch (executeFile fp True args Nothing) (\_ -> return ())
                     exitFailure
        in
        do 
        System.Posix.Signals.installHandler
                      System.Posix.Signals.sigPIPE
                      System.Posix.Signals.Ignore
                      Nothing
        p <- try (forkProcess childstuff)
        pid <- case p of
                Right x -> return x
                Left e -> fail ("Error in fork: " ++ (show e))
        retval <- func $! pid
        let rv = seq retval retval
        status <- getProcessStatus True False $! (seq retval pid)
        case status of
           Nothing -> fail "Got no process status back"
           Just (Exited (ExitSuccess)) -> return rv
           Just (Exited (ExitFailure fc)) -> cmdfailed "pOpen3" fp args fc
           Just (Terminated sig) -> fail ("Command terminated by signal" ++ show sig)
           Just (Stopped sig) -> fail ("Command stopped by signal" ++ show sig)
