-- arch-tag: Command utilities main file
{-# LANGUAGE CPP #-}
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
   Copyright  : Copyright (C) 2004-2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable to platforms with POSIX process/signal tools

Command invocation utilities.

Written by John Goerzen, jgoerzen\@complete.org

Please note: Most of this module is not compatible with Hugs.

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

It is possible to set up pipelines with these utilities.  Example:

> (pid1, x1) <- pipeFrom "ls" ["/etc"]
> (pid2, x2) <- pipeBoth "grep" ["x"] x1
> putStr x2
> ... the grep output is displayed ...
> forceSuccess pid2
> forceSuccess pid1

Remember, when you use the functions that return a String, you must not call
'forceSuccess' until after all data from the String has been consumed.  Failure
to wait will cause your program to appear to hang.

Here is an example of the wrong way to do it:

> (pid, x) <- pipeFrom "ls" ["/etc"]
> forceSuccess pid         -- Hangs; the called program hasn't terminated yet
> processTheData x

You must instead process the data before calling 'forceSuccess'.

When using the hPipe family of functions, this is probably more obvious.

Most of this module will be incompatible with Windows.
-}


module MissingH.Cmd(-- * High-Level Tools
                    PipeHandle(..),
                    safeSystem,
#ifndef mingw32_HOST_OS
                    forceSuccess,
                    -- ** Piping with lazy strings
                    pipeFrom,
                    pipeLinesFrom,
                    pipeTo,
                    pipeBoth,
                    -- ** Piping with handles
                    hPipeFrom,
                    hPipeTo,
                    hPipeBoth,
#endif
                    -- * Low-Level Tools
                    PipeMode(..),
#ifndef mingw32_HOST_OS
                    pOpen, pOpen3
#endif
		   )
where

-- FIXME - largely obsoleted by 6.4 - convert to wrappers.

import System.Exit
import System.Cmd
import MissingH.Logging.Logger
#ifndef mingw32_HOST_OS
import System.Posix.IO
import System.Posix.Process
#endif
import System.Posix.Types
import System.IO
import System.IO.Error
import Control.Concurrent(forkIO)
import Control.Exception(finally)

import qualified System.Posix.Signals

data PipeMode = ReadFromPipe | WriteToPipe

logbase = "MissingH.Cmd"

{- | Return value from 'pipeFrom', 'pipeLinesFrom', 'pipeTo', or
'pipeBoth'.  Contains both a ProcessID and the original command that was
executed.  If you prefer not to use 'forceSuccess' on the result of one
of these pipe calls, you can use (processID ph), assuming ph is your 'PipeHandle',
as a parameter to 'System.Posix.Process.getProcessStatus'. -}
data PipeHandle = 
    PipeHandle { processID :: ProcessID,
                 phCommand :: FilePath,
                 phArgs :: [String],
                 phCreator :: String -- ^ Function that created it
               }
    deriving (Eq, Show)

#ifndef mingw32_HOST_OS
{- | Like 'pipeFrom', but returns data in lines instead of just a String.
Shortcut for calling lines on the result from 'pipeFrom'.

Note: this function logs as pipeFrom.

Not available on Windows. -}
pipeLinesFrom :: FilePath -> [String] -> IO (PipeHandle, [String])
pipeLinesFrom fp args =
    do (pid, c) <- pipeFrom fp args
       return $ (pid, lines c)
#endif

logRunning func fp args = debugM (logbase ++ "." ++ func) (showCmd fp args)
warnFail funcname fp args msg =
    let m = showCmd fp args ++ ": " ++ msg
        in do warningM (logbase ++ "." ++ funcname) m
              fail m

#ifndef mingw32_HOST_OS
{- | Read data from a pipe.  Returns a Handle and a 'PipeHandle'.

When done, you must hClose the handle, and then use either 'forceSuccess' or
getProcessStatus on the 'PipeHandle'.  Zomeibes will result otherwise.

This function logs as pipeFrom.

Not available on Windows.
-}
hPipeFrom :: FilePath -> [String] -> IO (PipeHandle, Handle)
hPipeFrom fp args = 
    do pipepair <- createPipe
       logRunning "pipeFrom" fp args
       let childstuff = do dupTo (snd pipepair) stdOutput
                           closeFd (fst pipepair)
                           executeFile fp True args Nothing
       p <- try (forkProcess childstuff)
       -- parent
       pid <- case p of
                  Right x -> return x
                  Left e -> warnFail "pipeFrom" fp args $ 
                            "Error in fork: " ++ show e
       closeFd (snd pipepair)
       h <- fdToHandle (fst pipepair)
       return (PipeHandle pid fp args "pipeFrom", h)
#endif

#ifndef mingw32_HOST_OS
{- | Read data from a pipe.  Returns a lazy string and a 'PipeHandle'.

ONLY AFTER the string has been read completely, You must call either
'System.Posix.Process.getProcessStatus' or 'forceSuccess' on the 'PipeHandle'.
Zombies will result otherwise.

Not available on Windows.
-}
pipeFrom :: FilePath -> [String] -> IO (PipeHandle, String)
pipeFrom fp args =
    do (pid, h) <- hPipeFrom fp args
       c <- hGetContents h
       return (pid, c)
#endif

#ifndef mingw32_HOST_OS
{- | Write data to a pipe.  Returns a 'PipeHandle' and a new Handle to write
to.

When done, you must hClose the handle, and then use either 'forceSuccess' or
getProcessStatus on the 'PipeHandle'.  Zomeibes will result otherwise.

This function logs as pipeTo.

Not available on Windows.
-}
hPipeTo :: FilePath -> [String] -> IO (PipeHandle, Handle)
hPipeTo fp args =
    do pipepair <- createPipe
       logRunning "pipeTo" fp args
       let childstuff = do dupTo (fst pipepair) stdInput
                           closeFd (snd pipepair)
                           executeFile fp True args Nothing
       p <- try (forkProcess childstuff)
       -- parent
       pid <- case p of
                   Right x -> return x
                   Left e -> warnFail "pipeTo" fp args $ 
                             "Error in fork: " ++ show e
       closeFd (fst pipepair)
       h <- fdToHandle (snd pipepair)
       return (PipeHandle pid fp args "pipeTo", h)
#endif

#ifndef mingw32_HOST_OS
{- | Write data to a pipe.  Returns a ProcessID.

You must call either
'System.Posix.Process.getProcessStatus' or 'forceSuccess' on the ProcessID.
Zombies will result otherwise.

Not available on Windows.
-}
pipeTo :: FilePath -> [String] -> String -> IO PipeHandle
pipeTo fp args message =
    do (pid, h) <- hPipeTo fp args
       finally (hPutStr h message)
               (hClose h)
       return pid
#endif

#ifndef mingw32_HOST_OS
{- | Like a combination of 'hPipeTo' and 'hPipeFrom'; returns
a 3-tuple of ('PipeHandle', Data From Pipe, Data To Pipe).

When done, you must hClose both handles, and then use either 'forceSuccess' or
getProcessStatus on the 'PipeHandle'.  Zomeibes will result otherwise.

Hint: you will usually need to ForkIO a thread to handle one of the Handles;
otherwise, deadlock can result.

This function logs as pipeBoth.

Not available on Windows.
-}
hPipeBoth :: FilePath -> [String] -> IO (PipeHandle, Handle, Handle)
hPipeBoth fp args =
    do frompair <- createPipe
       topair <- createPipe
       logRunning "pipeBoth" fp args
       let childstuff = do dupTo (snd frompair) stdOutput
                           closeFd (fst frompair)
                           dupTo (fst topair) stdInput
                           closeFd (snd topair)
                           executeFile fp True args Nothing
       p <- try (forkProcess childstuff)
       -- parent
       pid <- case p of
                   Right x -> return x
                   Left e -> warnFail "pipeBoth" fp args $
                             "Error in fork: " ++ show e
       closeFd (snd frompair)
       closeFd (fst topair)
       fromh <- fdToHandle (fst frompair)
       toh <- fdToHandle (snd topair)
       return (PipeHandle pid fp args "pipeBoth", fromh, toh)
#endif

#ifndef mingw32_HOST_OS
{- | Like a combination of 'pipeTo' and 'pipeFrom'; forks an IO thread
to send data to the piped program, and simultaneously returns its output
stream.

The same note about checking the return status applies here as with 'pipeFrom'.

Not available on Windows. -}
pipeBoth :: FilePath -> [String] -> String -> IO (PipeHandle, String)
pipeBoth fp args message =
    do (pid, fromh, toh) <- hPipeBoth fp args
       forkIO $ finally (hPutStr toh message)
                        (hClose toh)
       c <- hGetContents fromh
       return (pid, c)
#endif

#ifndef mingw32_HOST_OS
{- | Uses 'System.Posix.Process.getProcessStatus' to obtain the exit status
of the given process ID.  If the process terminated normally, does nothing.
Otherwise, raises an exception with an appropriate error message.

This call will block waiting for the given pid to terminate. 

Not available on Windows. -}
forceSuccess :: PipeHandle -> IO ()
forceSuccess (PipeHandle pid fp args funcname) =
    let warnfail = warnFail funcname
        in do status <- getProcessStatus True False pid
              case status of
                Nothing -> warnfail fp args $ "Got no process status"
                Just (Exited (ExitSuccess)) -> return ()
                Just (Exited (ExitFailure fc)) -> 
                    cmdfailed funcname fp args fc
                Just (Terminated sig) -> 
                    warnfail fp args $ "Terminated by signal " ++ show sig
                Just (Stopped sig) -> 
                    warnfail fp args $ "Stopped by signal " ++ show sig 
#endif

{- | Invokes the specified command in a subprocess, waiting for the result.
If the command terminated successfully, return normally.  Otherwise,
raises a userError with the problem.
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

#ifndef mingw32_HOST_OS
{- | Open a pipe to the specified command.

Passes the handle on to the specified function.

The 'PipeMode' specifies what you will be doing.  That is, specifing 'ReadFromPipe' 
sets up a pipe from stdin, and 'WriteToPipe' sets up a pipe from stdout.

Not available on Windows.
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
#endif

#ifndef mingw32_HOST_OS
{- | Runs a command, redirecting things to pipes. 

Not available on Windows.-}
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
        forceSuccess (PipeHandle (seq retval pid) fp args "pOpen3")
        return rv
#endif

showCmd :: FilePath -> [String] -> String
showCmd fp args =
    fp ++ " " ++ show args
