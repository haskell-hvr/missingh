{- 
Copyright (c) 2005-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : MissingH.getOpt
   Copyright  : Copyright (C) 2005-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Written by John Goerzen, jgoerzen\@complete.org

Utilities for command-line parsing, including wrappers around
the standard System.Console.GetOpt module.
-}
module System.Console.GetOpt.Utils (parseCmdLine,
                        validateCmdLine,
                        StdOption,
                        stdRequired,
                        stdOptional
                       )
where
import System.Console.GetOpt
import System.Environment

{- | Simple command line parser -- a basic wrapper around the system's
default getOpt.  See the System.Console.GetOpt manual for a description of the
first two parameters.

The third parameter is a usage information header.

The return value consists of the list of parsed flags and a list of
non-option arguments. -}
parseCmdLine :: ArgOrder a -> [OptDescr a] -> String -> IO ([a], [String])
parseCmdLine order options header = 
    do argv <- getArgs
       case getOpt order options argv of
         (o, n, []) -> return (o, n)
         (_, _, errors) -> ioError (userError (concat errors ++ 
                                               usageInfo header options))

{- | Similar to 'parseCmdLine', but takes an additional function that validates
the post-parse command-line arguments.  This is useful, for example, in
situations where there are two arguments that are mutually-exclusive and only
one may legitimately be given at a time.

The return value of the function indicates whether or not it detected an
error condition.  If it returns Nothing, there is no error.  If it returns
Just String, there was an error, described by the String.
-}
validateCmdLine :: ArgOrder a -> [OptDescr a] -> String ->
                   (([a],[String]) -> Maybe String) -> IO ([a], [String])
validateCmdLine order options header func =
    do res <- parseCmdLine order options header
       case func res of
         Nothing -> return res
         Just errormsg -> ioError (userError (errormsg ++ "\n" ++
                                              usageInfo header options))

{- | A type to standardize some common uses of GetOpt.

The first component of the tuple is the long name of the option.

The second component is empty if there is no arg, or has the arg otherwise. -}
type StdOption = (String, String)

{- | Handle a required argument. -}
stdRequired :: String           -- ^ Name of arg
            -> String -> StdOption
stdRequired name value = (name, value)

{- | Handle an optional argument. -}
stdOptional :: String           -- ^ Name of arg
               -> Maybe String -> StdOption
stdOptional name Nothing = (name, "")
stdOptional name (Just x) = (name, x)
