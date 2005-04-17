-- arch-tag: Command utilities main file
{-
Copyright (C) 2004-2005 John Goerzen <jgoerzen@complete.org>

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

module MissingH.Cmd(-- * High-Level Tools
                    safeSystem,
                    forceSuccess,
                    pipeFrom,
                    pipeLinesFrom,
                    pipeTo,
                    pipeBoth,
                    -- * Low-Level Tools
                    PipeMode(..),
                    pOpen, pOpen3)
where

-- FIXME - largely obsoleted by 6.4 - convert to wrappers.

-- Note: This module is not compatible with Hugs.

import System.Exit
import System.Cmd
import MissingH.Logging.Logger
import System.Posix.IO
import System.Posix.Process
import System.Posix.Types
import System.IO
import Control.Concurrent(forkIO)
import Control.Exception(finally)

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

{- | Like 'pipeFrom', but returns data in lines instead of just a String.
Shortcut for calling lines on the result from 'pipeFrom'. -}
pipeLinesFrom :: FilePath -> [String] -> IO (ProcessID, [String])
pipeLinesFrom fp args =
    do (pid, c) <- pipeFrom fp args
       return $ (pid, lines c)

{- | Read data from a pipe.  Returns a lazy string and a ProcessID.

ONLY AFTER the string has been read completely, You must call either
'System.Posix.Process.getProcessStatus' or 'forceSuccess' on the ProcessID.
Zombies will result otherwise.
-}
pipeFrom :: FilePath -> [String] -> IO (ProcessID, String)
pipeFrom fp args =
    do pipepair <- createPipe
       debugM (logbase ++ ".pipeFrom")
              ("Running: " ++ fp ++ " " ++ (show args))
       let childstuff = do dupTo (snd pipepair) stdOutput
                           executeFile fp True args Nothing
       p <- try (forkProcess childstuff)
       -- parent
       pid <- case p of
                     Right x -> return x
                     Left e -> fail $ "Error in fork: " ++ show e
       closeFd (snd pipepair)
       h <- fdToHandle (fst pipepair)
       c <- hGetContents h
       return (pid, c)

{- | Write data to a pipe.  Returns a ProcessID.

You must call either
'System.Posix.Process.getProcessStatus' or 'forceSuccess' on the ProcessID.
Zombies will result otherwise.
-}
pipeTo :: FilePath -> [String] -> String -> IO ProcessID
pipeTo fp args message =
    do pipepair <- createPipe
       debugM (logbase ++ ".pipeto")
              ("Running: " ++ fp ++ " " ++ show args)
       let childstuff = do dupTo (fst pipepair) stdInput
                           executeFile fp True args Nothing
       p <- try (forkProcess childstuff)
       -- parent
       pid <- case p of
                     Right x -> return x
                     Left e -> fail $ "Error in fork: " ++ show e
       closeFd (fst pipepair)
       h <- fdToHandle (snd pipepair)
       finally (hPutStr h message)
               (hClose h)
       return pid

{- | Like a combination of 'pipeTo' and 'pipeFrom'; forks an IO thread
to send data to the piped program, and simultaneously returns its output
stream.

The same caveat about checking the return status applies here as with 'pipeFrom'. -}
pipeBoth :: FilePath -> [String] -> String -> IO (ProcessID, String)
pipeBoth fp args message =
    do frompair <- createPipe
       topair <- createPipe
       debugM (logbase ++ ".pipeBoth")
              ("Running: " ++ fp ++ " " ++ show args)
       let childstuff = do dupTo (snd frompair) stdOutput
                           dupTo (fst topair) stdInput
                           executeFile fp True args Nothing
       p <- try (forkProcess childstuff)
       -- parent
       pid <- case p of
                     Right x -> return x
                     Left e -> fail $ "Error in fork: " ++ show e
       closeFd (snd frompair)
       closeFd (fst topair)
       fromh <- fdToHandle (fst frompair)
       toh <- fdToHandle (snd topair)
       forkIO $ finally (hPutStr toh message)
                        (hClose toh)
       c <- hGetContents fromh
       return (pid, c)

{- | Uses 'System.Posix.Process.getProcessStatus' to obtain the exit status
of the given process ID.  If the process terminated normally, does nothing.
Otherwise, raises an exception with an appropriate error message.  If error
text is given, it is appended to the error message in the exception.

This call will block waiting for the given pid to terminate. -}
forceSuccess :: ProcessID -> Maybe String -> IO ()
forceSuccess pid errormsg_m =
    let errormsg = case errormsg_m of
                                 Just x -> ": " ++ x
                                 Nothing -> ""
        in do status <- getProcessStatus True False pid
              case status of
                Nothing -> fail $ "Got no process status" ++ errormsg
                Just (Exited (ExitSuccess)) -> return ()
                Just (Exited (ExitFailure fc)) -> fail $ "Exit failure code " ++ 
                                                  (show fc) ++ errormsg
                Just (Terminated sig) -> fail $ "Terminated by signal " ++ show sig ++
                                         errormsg
                Just (Stopped sig) -> fail $ "Stopped by signal " ++ show sig ++
                                      errormsg



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
        debugM (logbase ++ ".pOpen")
               ("Running: " ++ fp ++ " " ++ (show args))
        case pm of
         ReadFromPipe -> do
                         let callfunc _ = do
                                        closeFd (snd pipepair)
                                        h <- fdToHandle (fst pipepair)
                                        x <- func h
                                        hClose h
                                        return $! x
                         pOpen3 Nothing (Just (snd pipepair)) Nothing fp args
                                callfunc (closeFd (fst pipepair))
         WriteToPipe -> do 
                        let callfunc _ = do
                                       closeFd (fst pipepair)
                                       h <- fdToHandle (snd pipepair)
                                       x <- func h
                                       hClose h
                                       return $! x
                        pOpen3 (Just (fst pipepair)) Nothing Nothing fp args
                               callfunc (closeFd (snd pipepair))

{- | Runs a command, redirecting things to pipes. -}
pOpen3 :: Maybe Fd                      -- ^ Send stdin to this fd
       -> Maybe Fd                      -- ^ Get stdout from this fd
       -> Maybe Fd                      -- ^ Get stderr from this fd
       -> FilePath                      -- ^ Command to run
       -> [String]                      -- ^ Command args
       -> (ProcessID -> IO a)           -- ^ Action to run in parent
       -> IO ()                         -- ^ Action to run in child before execing (if you don't need something, set this to @return ()@) -- IGNORED IN HUGS
       -> IO a
pOpen3 pin pout perr fp args func childfunc = 
    let mayberedir Nothing _ = return ()
        mayberedir (Just fromfd) tofd = do
                                        dupTo fromfd tofd
                                        closeFd fromfd
                                        return ()
        childstuff = do
                     mayberedir pin stdInput
                     mayberedir pout stdOutput
                     mayberedir perr stdError
                     childfunc
                     debugM (logbase ++ ".pOpen3")
                            ("Running: " ++ fp ++ " " ++ (show args))
                     executeFile fp True args Nothing
        realfunc p = do
                     System.Posix.Signals.installHandler
                           System.Posix.Signals.sigPIPE
                           System.Posix.Signals.Ignore
                           Nothing
                     func p
        in
        do 
        p <- try (forkProcess childstuff)
        pid <- case p of
                Right x -> return x
                Left e -> fail ("Error in fork: " ++ (show e))
        retval <- func $! pid
        let rv = seq retval retval
        forceSuccess (seq retval pid) $ Just $ fp ++ " " ++ show args
        return rv

