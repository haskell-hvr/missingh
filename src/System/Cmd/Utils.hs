-- arch-tag: Command utilities main file
{-# LANGUAGE CPP #-}
{-
Copyright (c) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : System.Cmd.Utils
   Copyright  : Copyright (C) 2004-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable to platforms with POSIX process\/signal tools

Command invocation utilities.

Written by John Goerzen, jgoerzen\@complete.org

Please note: Most of this module is not compatible with Hugs.

Command lines executed will be logged using "System.Log.Logger" at the
DEBUG level.  Failure messages will be logged at the WARNING level in addition
to being raised as an exception.  Both are logged under
\"System.Cmd.Utils.funcname\" -- for instance,
\"System.Cmd.Utils.safeSystem\".  If you wish to suppress these messages
globally, you can simply run:

> updateGlobalLogger "System.Cmd.Utils.safeSystem"
>                     (setLevel CRITICAL)

See also: 'System.Log.Logger.updateGlobalLogger',
"System.Log.Logger".

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


module System.Cmd.Utils(-- * High-Level Tools
                    PipeHandle(..),
                    safeSystem,
#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
                    forceSuccess,
#ifndef __HUGS__
                    posixRawSystem,
                    forkRawSystem,
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
#endif
                    -- * Low-Level Tools
                    PipeMode(..),
#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
#ifndef __HUGS__
                    pOpen, pOpen3, pOpen3Raw
#endif
#endif
                   )
where

-- FIXME - largely obsoleted by 6.4 - convert to wrappers.

import System.Exit
import System.Cmd
import System.Log.Logger
#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
import System.Posix.IO
import System.Posix.Process
import System.Posix.Signals
import qualified System.Posix.Signals
#endif
import System.Posix.Types
import System.IO
import System.IO.Error
import Control.Concurrent(forkIO)
import Control.Exception(finally)
import qualified Control.Exception(try, IOException)

data PipeMode = ReadFromPipe | WriteToPipe

logbase :: String
logbase = "System.Cmd.Utils"

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

#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
#ifndef __HUGS__
{- | Like 'pipeFrom', but returns data in lines instead of just a String.
Shortcut for calling lines on the result from 'pipeFrom'.

Note: this function logs as pipeFrom.

Not available on Windows. -}
pipeLinesFrom :: FilePath -> [String] -> IO (PipeHandle, [String])
pipeLinesFrom fp args =
    do (pid, c) <- pipeFrom fp args
       return $ (pid, lines c)
#endif
#endif

logRunning :: String -> FilePath -> [String] -> IO ()
logRunning func fp args = debugM (logbase ++ "." ++ func) (showCmd fp args)

warnFail :: [Char] -> FilePath -> [String] -> [Char] -> IO t
warnFail funcname fp args msg =
    let m = showCmd fp args ++ ": " ++ msg
        in do warningM (logbase ++ "." ++ funcname) m
              fail m

#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
#ifndef __HUGS__
{- | Read data from a pipe.  Returns a Handle and a 'PipeHandle'.

When done, you must hClose the handle, and then use either 'forceSuccess' or
getProcessStatus on the 'PipeHandle'.  Zombies will result otherwise.

This function logs as pipeFrom.

Not available on Windows or with Hugs.
-}
hPipeFrom :: FilePath -> [String] -> IO (PipeHandle, Handle)
hPipeFrom fp args =
    do pipepair <- createPipe
       logRunning "pipeFrom" fp args
       let childstuff = do dupTo (snd pipepair) stdOutput
                           closeFd (fst pipepair)
                           executeFile fp True args Nothing
       p <- Control.Exception.try (forkProcess childstuff)
       -- parent
       pid <- case p of
                  Right x -> return x
                  Left (e :: Control.Exception.IOException) -> warnFail "pipeFrom" fp args $
                            "Error in fork: " ++ show e
       closeFd (snd pipepair)
       h <- fdToHandle (fst pipepair)
       return (PipeHandle pid fp args "pipeFrom", h)
#endif
#endif

#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
#ifndef __HUGS__
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
#endif

#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
#ifndef __HUGS__
{- | Write data to a pipe.  Returns a 'PipeHandle' and a new Handle to write
to.

When done, you must hClose the handle, and then use either 'forceSuccess' or
getProcessStatus on the 'PipeHandle'.  Zombies will result otherwise.

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
       p <- Control.Exception.try (forkProcess childstuff)
       -- parent
       pid <- case p of
                   Right x -> return x
                   Left (e :: Control.Exception.IOException) -> warnFail "pipeTo" fp args $
                             "Error in fork: " ++ show e
       closeFd (fst pipepair)
       h <- fdToHandle (snd pipepair)
       return (PipeHandle pid fp args "pipeTo", h)
#endif
#endif

#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
#ifndef __HUGS__
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
#endif

#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
#ifndef __HUGS__
{- | Like a combination of 'hPipeTo' and 'hPipeFrom'; returns
a 3-tuple of ('PipeHandle', Data From Pipe, Data To Pipe).

When done, you must hClose both handles, and then use either 'forceSuccess' or
getProcessStatus on the 'PipeHandle'.  Zombies will result otherwise.

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
       p <- Control.Exception.try (forkProcess childstuff)
       -- parent
       pid <- case p of
                   Right x -> return x
                   Left (e :: Control.Exception.IOException) -> warnFail "pipeBoth" fp args $
                             "Error in fork: " ++ show e
       closeFd (snd frompair)
       closeFd (fst topair)
       fromh <- fdToHandle (fst frompair)
       toh <- fdToHandle (snd topair)
       return (PipeHandle pid fp args "pipeBoth", fromh, toh)
#endif
#endif

#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
#ifndef __HUGS__
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
#endif

#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
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

Implemented in terms of 'posixRawSystem' where supported, and System.Posix.rawSystem otherwise.
-}
safeSystem :: FilePath -> [String] -> IO ()
safeSystem command args =
    do debugM (logbase ++ ".safeSystem")
               ("Running: " ++ command ++ " " ++ (show args))
#if defined(__HUGS__) || defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__)
       ec <- rawSystem command args
       case ec of
            ExitSuccess -> return ()
            ExitFailure fc -> cmdfailed "safeSystem" command args fc
#else
       ec <- posixRawSystem command args
       case ec of
            Exited ExitSuccess -> return ()
            Exited (ExitFailure fc) -> cmdfailed "safeSystem" command args fc
            Terminated s -> cmdsignalled "safeSystem" command args s
            Stopped s -> cmdsignalled "safeSystem" command args s
#endif

#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
#ifndef __HUGS__
{- | Invokes the specified command in a subprocess, waiting for the result.
Return the result status.  Never raises an exception.  Only available
on POSIX platforms.

Like system(3), this command ignores SIGINT and SIGQUIT and blocks SIGCHLD
during its execution.

Logs as System.Cmd.Utils.posixRawSystem -}
posixRawSystem :: FilePath -> [String] -> IO ProcessStatus
posixRawSystem program args =
    do debugM (logbase ++ ".posixRawSystem")
               ("Running: " ++ program ++ " " ++ (show args))
       oldint <- installHandler sigINT Ignore Nothing
       oldquit <- installHandler sigQUIT Ignore Nothing
       let sigset = addSignal sigCHLD emptySignalSet
       oldset <- getSignalMask
       blockSignals sigset
       childpid <- forkProcess (childaction oldint oldquit oldset)

       mps <- getProcessStatus True False childpid
       restoresignals oldint oldquit oldset
       let retval = case mps of
                      Just x -> x
                      Nothing -> error "Nothing returned from getProcessStatus"

       debugM (logbase ++ ".posixRawSystem")
              (program ++ ": exited with " ++ show retval)
       return retval

    where childaction oldint oldquit oldset =
              do restoresignals oldint oldquit oldset
                 executeFile program True args Nothing
          restoresignals oldint oldquit oldset =
              do installHandler sigINT oldint Nothing
                 installHandler sigQUIT oldquit Nothing
                 setSignalMask oldset

#endif
#endif

#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
#ifndef __HUGS__
{- | Invokes the specified command in a subprocess, without waiting for
the result.  Returns the PID of the subprocess -- it is YOUR responsibility
to use getProcessStatus or getAnyProcessStatus on that at some point.  Failure
to do so will lead to resource leakage (zombie processes).

This function does nothing with signals.  That too is up to you.

Logs as System.Cmd.Utils.forkRawSystem -}
forkRawSystem :: FilePath -> [String] -> IO ProcessID
forkRawSystem program args =
    do debugM (logbase ++ ".forkRawSystem")
               ("Running: " ++ program ++ " " ++ (show args))
       forkProcess childaction
    where
      childaction = executeFile program True args Nothing

#endif
#endif

cmdfailed :: String -> FilePath -> [String] -> Int -> IO a
cmdfailed funcname command args failcode = do
    let errormsg = "Command " ++ command ++ " " ++ (show args) ++
            " failed; exit code " ++ (show failcode)
    let e = userError (errormsg)
    warningM (logbase ++ "." ++ funcname) errormsg
    ioError e

#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
#ifndef __HUGS__
cmdsignalled :: String -> FilePath -> [String] -> Signal -> IO a
cmdsignalled funcname command args failcode = do
    let errormsg = "Command " ++ command ++ " " ++ (show args) ++
            " failed due to signal " ++ (show failcode)
    let e = userError (errormsg)
    warningM (logbase ++ "." ++ funcname) errormsg
    ioError e
#endif
#endif

#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
#ifndef __HUGS__
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
#endif

#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
#ifndef __HUGS__
{- | Runs a command, redirecting things to pipes.

Not available on Windows.

Note that you may not use the same fd on more than one item.  If you
want to redirect stdout and stderr, dup it first.
-}
pOpen3 :: Maybe Fd                      -- ^ Send stdin to this fd
       -> Maybe Fd                      -- ^ Get stdout from this fd
       -> Maybe Fd                      -- ^ Get stderr from this fd
       -> FilePath                      -- ^ Command to run
       -> [String]                      -- ^ Command args
       -> (ProcessID -> IO a)           -- ^ Action to run in parent
       -> IO ()                         -- ^ Action to run in child before execing (if you don't need something, set this to @return ()@) -- IGNORED IN HUGS
       -> IO a
pOpen3 pin pout perr fp args func childfunc =
    do pid <- pOpen3Raw pin pout perr fp args childfunc
       retval <- func $! pid
       let rv = seq retval retval
       forceSuccess (PipeHandle (seq retval pid) fp args "pOpen3")
       return rv
#endif
#endif

#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
#ifndef __HUGS__
{- | Runs a command, redirecting things to pipes.

Not available on Windows.

Returns immediately with the PID of the child.  Using 'waitProcess' on it
is YOUR responsibility!

Note that you may not use the same fd on more than one item.  If you
want to redirect stdout and stderr, dup it first.
-}
pOpen3Raw :: Maybe Fd                      -- ^ Send stdin to this fd
       -> Maybe Fd                      -- ^ Get stdout from this fd
       -> Maybe Fd                      -- ^ Get stderr from this fd
       -> FilePath                      -- ^ Command to run
       -> [String]                      -- ^ Command args
       -> IO ()                         -- ^ Action to run in child before execing (if you don't need something, set this to @return ()@) -- IGNORED IN HUGS
       -> IO ProcessID
pOpen3Raw pin pout perr fp args childfunc =
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
{-
        realfunc p = do
                     System.Posix.Signals.installHandler
                           System.Posix.Signals.sigPIPE
                           System.Posix.Signals.Ignore
                           Nothing
                     func p
-}
        in
        do
        p <- Control.Exception.try (forkProcess childstuff)
        pid <- case p of
                Right x -> return x
                Left (e :: Control.Exception.IOException) -> fail ("Error in fork: " ++ (show e))
        return pid

#endif
#endif

showCmd :: FilePath -> [String] -> String
showCmd fp args = fp ++ " " ++ show args
