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

This module contains extensive documentation.  Please scroll down to the Introduction section to continue reading.
-}
module MissingH.ConfigParser
    (
     -- * Introduction
     -- $introduction

     -- ** Features
     -- $features

     -- ** History
     -- $history

     -- * Configuration File Format
     -- $format

     -- ** White Space
     -- $whitespace

     -- ** Comments
     -- $comments

     -- ** Case Sensitivity
     -- $casesens

     -- ** Interpolation
     -- $interpolation

     -- * Usage Examples
     -- $usage

     -- ** Non-Monadic Usage
     -- $usagenomonad

     -- ** Error Monad Usage
     -- $usageerrormonad

     -- ** Combined Error\/IO Monad Usage
     -- $usageerroriomonad

     -- * Types
     -- $types
     SectionSpec, OptionSpec, ConfigParser(..),
     CPErrorData(..), CPError,
     -- * Initialization
     -- $initialization
     emptyCP,

     -- * Configuring the ConfigParser
     -- $configuringcp
     
     -- ** Access Functions
     simpleAccess, interpolatingAccess,

     -- * Reading
     -- $reading
     readfile, readhandle, readstring,

     -- * Accessing Data
     Get_C(..),
     sections, has_section,
     options, has_option,
     items,

     -- * Modifying Data
     set, setshow, remove_option,
     add_section, remove_section,
     merge,

     -- * Output Data
     to_string


) where
import MissingH.ConfigParser.Types
import MissingH.ConfigParser.Parser
import MissingH.FiniteMap
import MissingH.Either
import MissingH.Str
import Data.FiniteMap
import Data.List
import System.IO(Handle)
import Data.Char
import Control.Monad.Error

-- For interpolatingAccess
import Text.ParserCombinators.Parsec.Error(ParseError, messageString,
    errorMessages, Message(..))
import Text.ParserCombinators.Parsec(parse)

----------------------------------------------------------------------
-- Basic types / default values
----------------------------------------------------------------------

{- | The default empty 'MissingH.ConfigParser' object.

The content contains only an empty mandatory @DEFAULT@ section.

'optionxform' is set to @map toLower@.

'usedefault' is set to @True@.

'accessfunc' is set to 'simpleAccess'.
-}
emptyCP :: ConfigParser
emptyCP = ConfigParser { content = fromAL [("DEFAULT", [])],
                       defaulthandler = defdefaulthandler,
                       optionxform = map toLower,
                       usedefault = True,
                       accessfunc = simpleAccess}

{- | Low-level tool to convert a parsed object into a 'CPData'
representation.  Performs no option conversions or special handling
of @DEFAULT@. -}
fromAL :: ParseOutput -> CPData
fromAL origal =
    let conv :: CPData -> (String, [(String, String)]) -> CPData
        conv fm sect = addToFM fm (fst sect) (listToFM $ snd sect)
        in
        foldl conv emptyFM origal

{- | Default (non-interpolating) access function -}
simpleAccess ::  MonadError CPError m =>
                 ConfigParser -> SectionSpec -> OptionSpec -> m String
simpleAccess cp s o = defdefaulthandler cp s (optionxform cp $ o)

{- | Interpolating access function.  Please see the Interpolation section
above for a background on interpolation.

Although the format string looks similar to one used by "MissingH.Printf",
it is not the same.  In particular, only the %(...)s format is supported.
No width specifiers are supported and no conversions other than s are supported.

To use this function, you must specify a maximum recursion depth for
interpolation.  This is used to prevent a stack overflow in the event that
the configuration file contains an endless interpolation loop.  Values of 10
or so are usually more than enough, though you could probably go into the
hundreds or thousands before you have actual problems.

A value less than one will cause an instant error every time you attempt
a lookup.

This access method can cause 'get' and friends to return a new 'CPError':
'InterpolationError'.  This error would be returned when:

 * The configuration file makes a reference to an option that does
   not exist

 * The maximum interpolation depth is exceeded

 * There is a syntax error processing a %-directive in the configuration
   file

An interpolation lookup name specifies an option only.  There is no provision
to specify a section.  Interpolation variables are looked up in the current
section, and, if 'usedefault' is True, in @DEFAULT@ according to the normal
logic.

To use a literal percent sign, you must place @%%@ in the configuration
file when interpolation is used.

Here is how you might enable interpolation:

>let cp2 = cp {accessfunc = interpolatingAccess 10}

The @cp2@ object will now support interpolation with a maximum depth of 10.
 -}
interpolatingAccess :: MonadError CPError m =>
                       Int ->
                       ConfigParser -> SectionSpec -> OptionSpec
                       -> m String

interpolatingAccess maxdepth cp s o =
    if maxdepth < 1
       then interError "maximum interpolation depth exceeded"
       else do x <- simpleAccess cp s o
               case parse (interpmain $ lookupfunc) (s ++ "/" ++ o) x of
                 Left y -> case head (errorMessages y) of
                                Message z -> interError z
                                _ -> interError (show y)
                 Right y -> return y
    where
    lookupfunc = interpolatingAccess (maxdepth - 1) cp s
    interError x = throwError (InterpolationError x, "interpolatingAccess")

-- internal function: default handler
defdefaulthandler ::  MonadError CPError m =>
                      ConfigParser -> SectionSpec -> OptionSpec -> m String

defdefaulthandler cp sect opt = 
    let fm = content cp
        lookup s o = do sect <- maybeToEither (NoSection s, "get") $ lookupFM fm s
                        maybeToEither (NoOption o, "get") $ lookupFM sect o
        trydefault e = if (usedefault cp)
                       then 
                            lookup "DEFAULT" opt 
                                       -- Use original error if it's not in DEFAULT either
                                       `catchError` (\_ -> throwError e)
                       else throwError e
        in 
        lookup sect opt `catchError` trydefault


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
	dest { content = plusFM (mapFM convFM (content src)) (content dest) }

{- | Utility to do a special case merge. -}
readutil :: ConfigParser -> ParseOutput -> ConfigParser
readutil old new = merge old $ old { content = fromAL new }

{- | Loads data from the specified file.  It is then combined with the
given 'ConfigParser' using the semantics documented under 'merge' with the
new data taking precedence over the old.  However, unlike
'merge', all the options
as set in the old object are preserved since the on-disk representation
does not convey those options.

May return an error if there is a syntax error.  May raise an exception if the file could not be accessed.
-}
--readfile :: ConfigParser -> FilePath ->IO (CPResult ConfigParser)
readfile :: MonadError CPError m => ConfigParser -> FilePath -> IO (m ConfigParser)
{-
readfile cp fp = do n <- parse_file fp
                    return $ do y <- n
                                return $ readutil cp y
-}
readfile cp fp = do n <- parse_file fp
                    return $ n >>= (return . readutil cp)

{- | Like 'readfile', but uses an already-open handle.  You should
use 'readfile' instead of this if possible, since it will be able to
generate better error messages.

Errors would be returned on a syntax error.
-}
--readhandle :: ConfigParser -> Handle -> IO (CPResult ConfigParser)
readhandle :: MonadError CPError m => ConfigParser -> Handle -> IO (m ConfigParser)
readhandle cp h = do n <- parse_handle h
                     return $ n >>= (return . (readutil cp))

{- | Like 'readfile', but uses a string.  You should use 'readfile'
instead of this if you are processing a file, since it can generate
better error messages.

Errors would be returned on a syntax error.
-}
readstring ::  MonadError CPError m =>
               ConfigParser -> String -> m ConfigParser
readstring cp s = do
                  n <- parse_string s
                  return $ readutil cp n

{- | Returns a list of sections in your configuration file.  Never includes
the always-present section @DEFAULT@. -}
sections :: ConfigParser -> [SectionSpec]
sections = filter (/= "DEFAULT") . keysFM . content

{- | Indicates whether the given section exists.

No special @DEFAULT@ processing is done. -}
has_section :: ConfigParser -> SectionSpec -> Bool
has_section cp x = elemFM x (content cp)

{- | Adds the specified section name.  Returns a
'SectionAlreadyExists' error if the
section was already present.  Otherwise, returns the new 
'ConfigParser' object.-}
add_section ::  MonadError CPError m =>
                ConfigParser -> SectionSpec -> m ConfigParser
add_section cp s =
    if has_section cp s
       then throwError $ (SectionAlreadyExists s, "add_section")
       else return $ cp {content = addToFM (content cp) s emptyFM}

{- | Removes the specified section.  Returns a 'NoSection' error if
the section does not exist; otherwise, returns the new 'ConfigParser'
object.

This call may not be used to remove the @DEFAULT@ section.  Attempting to do
so will always cause a 'NoSection' error.
 -}
remove_section ::  MonadError CPError m =>
                   ConfigParser -> SectionSpec -> m ConfigParser
remove_section _ "DEFAULT" = throwError $ (NoSection "DEFAULT", "remove_section")
remove_section cp s = 
    if has_section cp s
       then return $ cp {content = delFromFM (content cp) s}
       else throwError $ (NoSection s, "remove_section")

{- | Removes the specified option.  Returns a 'NoSection' error if the
section does not exist and a 'NoOption' error if the option does not
exist.  Otherwise, returns the new 'ConfigParser' object.
-}
remove_option ::  MonadError CPError m =>
                  ConfigParser -> SectionSpec -> OptionSpec -> m ConfigParser
remove_option cp s passedo =
    do sectmap <- maybeToEither (NoSection s, "remove_option") $ lookupFM (content cp) s
       let o = (optionxform cp) passedo
       let newsect = delFromFM sectmap o
       let newmap = addToFM (content cp) s newsect
       if elemFM o sectmap
          then return $ cp {content = newmap}
          else throwError $ (NoOption o, "remove_option")

{- | Returns a list of the names of all the options present in the
given section.

Returns an error if the given section does not exist.
-}
options ::  MonadError CPError m =>
            ConfigParser -> SectionSpec -> m [OptionSpec]
options cp x = maybeToEither (NoSection x, "options") $ 
               do
               o <- lookupFM (content cp) x
               return $ keysFM o

{- | Indicates whether the given option is present.  Returns True
only if the given section is present AND the given option is present
in that section.  No special @DEFAULT@ processing is done.  No
exception could be raised or error returned.
-}
has_option :: ConfigParser -> SectionSpec -> OptionSpec -> Bool
has_option cp s o = 
    let c = content cp
        v = do secthash <- lookupFM c s
               return $ elemFM (optionxform cp $ o) secthash
        in maybe False id v

{- | The class representing the data types that can be returned by "get".
-}
class Get_C a where 
    {- | Retrieves a string from the configuration file.

When used in a context where a String is expected, returns that string verbatim.

When used in a context where a Bool is expected, parses the string to
a Boolean value (see logic below).

When used in a context where anything that is an instance of Read is expected,
calls read to parse the item.

An error will be returned of no such option could be found or if it could
not be parsed as a boolean (when returning a Bool).

When parsing to a Bool, strings are case-insentively converted as follows:

The following will produce a True value:

 * 1

 * yes

 * on

 * enabled

 * true

The following will produce a False value:

 * 0

 * no

 * off

 * disabled

 * false -}
    get :: MonadError CPError m => ConfigParser -> SectionSpec -> OptionSpec -> m a
                           
instance Get_C String where
    get cp s o = eitherToMonadError $ (accessfunc cp) cp s o

instance Get_C Bool where
    get = getbool

instance (Num t, Read t) => Get_C t where
    get = genericget

genericget cp s o = get cp s o >>= return . read

getbool ::  MonadError CPError m =>
            ConfigParser -> SectionSpec -> OptionSpec -> m Bool
getbool cp s o = 
    do val <- get cp s o
       case map toLower . strip $ val of
                  "1" -> return True
                  "yes" -> return True
                  "on" -> return True
                  "enabled" -> return True
                  "true" -> return True
                  "0" -> return False
                  "no" -> return False
                  "off" -> return False
                  "disabled" -> return False
                  "false" -> return False
                  _ -> throwError (ParseError $ "couldn't parse bool " ++
                                   val ++ " from " ++ s ++ "/" ++ o, "getbool")

{- | Returns a list of @(optionname, value)@ pairs representing the content
of the given section.  Returns an error the section is invalid. -}
items ::  MonadError CPError m =>
          ConfigParser -> SectionSpec -> m [(OptionSpec, String)]
items cp s = do fm <- maybeToEither (NoSection s, "items") $ lookupFM (content cp) s
                return $ fmToList fm

{- | Sets the option to a new value, replacing an existing one if it exists.

Returns an error if the section does not exist. -}
set ::  MonadError CPError m =>
        ConfigParser -> SectionSpec -> OptionSpec -> String -> m ConfigParser
set cp s passedo val = 
    do sectmap <- maybeToEither (NoSection s, "set") $ lookupFM (content cp) s
       let o = (optionxform cp) passedo
       let newsect = addToFM sectmap o val
       let newmap = addToFM (content cp) s newsect
       return $ cp { content = newmap}

{- | Sets the option to a new value, replacing an existing one if it exists.
It requires only a showable value as its parameter.
This can be used with bool values, as well as numeric ones.

Returns an error if the section does not exist. -}
setshow :: (Show a, MonadError CPError m) => 
           ConfigParser -> SectionSpec -> OptionSpec -> a -> m ConfigParser
setshow cp s o val = set cp s o (show val)

{- | Converts the 'ConfigParser' to a string representation that could be
later re-parsed by this module or modified by a human.

Note that this does not necessarily re-create a file that was originally
loaded.  Things may occur in a different order, comments will be removed,
etc.  The conversion makes an effort to make the result human-editable,
but it does not make an effort to make the result identical to the original
input.

The result is, however, guaranteed to parse the same as the original input.
 -}
to_string :: ConfigParser -> String
to_string cp = 
    let gen_option (key, value) = 
            key ++ ": " ++ (replace "\n" "\n    " value) ++ "\n"
        gen_section (sect, valfm) = -- gen a section, but omit DEFAULT if empty
            if (sect /= "DEFAULT") || (sizeFM valfm > 0)
               then "[" ++ sect ++ "]\n" ++
                        (concat $ map gen_option (fmToList valfm)) ++ "\n"
               else ""
        in
        concat $ map gen_section (fmToList (content cp))

----------------------------------------------------------------------
-- Docs
----------------------------------------------------------------------

{- $introduction

Many programs need configuration files. These configuration files are
typically used to configure certain runtime behaviors that need to be
saved across sessions. Various different configuration file formats
exist.

The ConfigParser module attempts to define a standard format that is
easy for the user to edit, easy for the programmer to work with, yet
remains powerful and flexible.
-}

{- $features

For the programmer, this module provides:

 * Simple calls to both read /and write/ configuration files

 * Call that can generate a string version of a file that is
   re-parsable by this module (useful for, for instance, sending the
   file down a network)

 * Segmented configuration files that let you separate configuration
   into distinct sections, each with its own namespace. This can be
   used to configure multiple modules in one file, to configure
   multiple instances of a single object, etc.

 * On-the-fly parsing of integer, boolean, float, multi-line string values,
   and anything else Haskell's read can deal with

 * It is possible to make a configuration file parsable by this
   module, the Unix shell, and\/or Unix make, though some feautres are,
   of course, not compatible with these other tools.

 * Syntax checking with error reporting including line numbers

 * Implemented in pure Haskell.  No dependencies on modules outside
   the standard library distributed with Haskell compilers or interpreters.
   All calls except those that read directly from a handle are pure calls
   and can be used outside the IO monad.

 * Comprehensive documentation

 * Extensible API

 * Complete compatibility with Python's ConfigParser module, or my
   ConfigParser module for OCaml, part of my MissingLib package.

For the user, this module provides:

 * Easily human-editable configuration files with a clear, concise,
   and consistent format

 * Configuration file format consistent with other familiar formats
   (\/etc\/passwd is a valid ConfigParser file)

 * No need to understand semantics of markup languages like XML
-}

{- $history

This module is based on Python's ConfigParser module at
<http://www.python.org/doc/current/lib/module-ConfigParser.html>.  I had
earlier developed an OCaml implementation as part of my MissingLib library
at <gopher://gopher.quux.org/devel/missinglib>.

While the API of these three modules is similar, and the aim is to preserve all
useful features of the original Python module, there are some differences
in the implementation details.  This module is a complete, clean re-implementation
in Haskell, not a Haskell translation of a Python program.  As such, the feature
set is slightly different.
-}

{- $format

The basic configuration file format resembles that of an old-style
Windows .INI file. Here are two samples:

>debug = yes
>inputfile = /etc/passwd
>names = Peter, Paul, Mary, George, Abrahaham, John, Bill, Gerald, Richard,
>        Franklin, Woodrow
>color = red 

This defines a file without any explicit section, so all items will
occur within the default section @DEFAULT@. The @debug@ option can be read
as a boolean or a string. The remaining items can be read as a string
only. The @names@ entry spans two lines -- any line starting with
whitespace, and containing something other than whitespace or
comments, is taken as a continuation of the previous line.

Here's another example: 

># Default options
>[DEFAULT]
>hostname: localhost 
># Options for the first file
>[file1]
>location: /usr/local
>user: Fred
>uid: 1000
>optionaltext: Hello, this  entire string is included 
>[file2]
>location: /opt
>user: Fred
>uid: 1001 

This file defines three sections. The @DEFAULT@ section specifies an
entry @hostname@. If you attempt to read the hostname option in any
section, and that section doesn't define @hostname@, you will get the
value from @DEFAULT@ instead. This is a nice time-saver. You can also
note that you can use colons instead of the = character to separate
option names from option entries.
-}

{- $whitespace

Whitespace (spaces, tabs, etc) is automatically stripped from the
beginning and end of all strings. Thus, users can insert whitespace
before\/after the colon or equal sign if they like, and it will be
automatically stripped.

Blank lines or lines consisting solely of whitespace are ignored. 

A line giving an option or a section name may not begin with white space.
This requirement is necessary so there is no ambiguity between such lines
and continuation lines for multi-line options.

-}

{- $comments

Comments are introduced with the pound sign @#@ or the semicolon @;@. They
cause the parser to ignore everything from that character to the end
of the line.

Comments /may not/ occur within the definitions of options; that is, you
may not place a comment in the middle of a line such as @user: Fred@. 
That is because the parser considers the comment characters part
of the string; otherwise, you'd be unable to use those characters in
your strings. You can, however, \"comment out\" options by putting the
comment character at the start of the line.

-}

{- $casesens

By default, section names are case-sensitive but option names are
not. The latter can be adjusted by adjusting 'optionxform'.  -}

{- $interpolation

Interpolation is an optional feature, disabled by default.  If you replace
the default 'accessfunc' ('simpleAccess') with 'interpolatingAccess',
then you get interpolation support with 'get' and the other 'get'-based functions.

As an example, consider the following file:

>arch = i386
>project = test
>filename = test_%(arch)s.c
>dir = /usr/src/%(filename)s 
>percent = 5%% 

With interpolation, you would get these results:

>get cp "DEFAULT" "filename" -> "test_i386.c"
>get cp "DEFAULT" "dir" -> "/usr/src/test_i386.c"
>get cp "DEFAULT" "percent" -> "5%"

For more details on interpolation, please see the documentation for the
'interpolatingAccess' function.
-}

{- $usage

The basic theory of working with ConfigParser is this:

 1. Parse or build a 'ConfigParser' object
 
 2. Work with it in one of several ways

 3. To make changes, you discard the original object and use a new one.
    Changes can be "chained" through one of several monads.

The default 'ConfigParser' object that you always start with is 'emptyCP'.
From here, you load data into it (merging data into the empty object),
set up structures yourself, or adjust options.

Let's take a look at some basic use cases.

-}

{- $usagenomonad
You'll notice that many functions in this module return a 
@MonadError 'CPError'@ over some
type.  Although its definition is not this simple, you can consider this to be
the same as returning @Either CPError a@.

That is, these functions will return @Left error@ if there's a problem
or @Right result@ if things are fine.  The documentation for individual
functions describes the specific circumstances in which an error may occur in
more detail.

Some people find it annoying to have to deal with errors manually.
You can transform errors into exceptions in your code by using 
'MissingH.Either.forceEither'.  Here's an example of this style of programming:

> import MissingH.Either
> do
>    val <- readfile emptyCP "/etc/foo.cfg"
>    let cp = forceEither val
>    putStrLn "Your setting is:"
>    putStrLn $ forceEither $ get cp "sect1" "opt1"

In short, you can just put @forceEither $@ in front of every call that returns
something that is a MonadError.
This is still a pure functional call, so it can be used outside
of the IO monads.  The exception, however, can only be caught in the IO
monad.

If you don't want to bother with 'forceEither', you can use the error monad.  It's simple and better... read on.
-}

{- $usageerrormonad

The return type is actually defined in terms of the Error monad, which is
itself based on the Either data type.

Here's a neat example of chaining together calls to build up a 'ConfigParser'
object:

>do let cp = emptyCP
>   cp <- add_section cp "sect1"
>   cp <- set cp "sect1" "opt1" "foo"
>   cp <- set cp "sect1" "opt2" "bar"
>   options cp "sect1"

The return value of this little snippet is @Right [\"opt1\", \"opt2\"]@.
(Note to beginners: unlike the IO monad, you /can/ escape from the Error
monad.)

Although it's not obvious, there actually was error checking there.  If
any of those calls would have generated an error, processing would have
stopped immediately and a @Left@ value would have been returned.  Consider
this example:

>do let cp = emptyCP
>   cp <- add_section cp "sect1"
>   cp <- set cp "sect1" "opt1" "foo"
>   cp <- set cp "sect2" "opt2" "bar"
>   options cp "sect1"

The return value from this is @Left ('NoSection' \"sect2\", \"set\")@.  The
second call to 'set' failed, so the final call was skipped, and the result
of the entire computation was considered to be an error.

You can combine this with the non-monadic style to get a final, pure value
out of it:

>forceEither $ do let cp = emptyCP
>                 cp <- add_section cp "sect1"
>                 cp <- set cp "sect1" "opt1" "foo"
>                 cp <- set cp "sect1" "opt2" "bar"
>                 options cp "sect1"

This returns @[\"opt1\", \"opt2\"]@.  A quite normal value.

-}

{- $usageerroriomonad

You've seen a nice way to use this module in the Error monad and get an Either
value out.  But that's the Error monad, so IO is not permitted.  
Using Haskell's monad transformers, you can run it in the combined
Error\/IO monad.  That is, you will get an IO result back.  Here is a full
standalone example of doing that:

>import MissingH.ConfigParser
>import Control.Monad.Error
>
>main = do
>          rv <- runErrorT $
>              do
>              cp <- join $ liftIO $ readfile empty "/etc/passwd"
>              let x = cp
>              liftIO $ putStrLn "In the test"
>              nb <- get x "DEFAULT" "nobody"
>              liftIO $ putStrLn nb
>              foo <- get x "DEFAULT" "foo"
>              liftIO $ putStrLn foo
>              return "done"
>          print rv

On my system, this prints:

>In the test
>x:65534:65534:nobody:/nonexistent:/bin/sh
>Left (NoOption "foo","get")

That is, my @\/etc\/passwd@ file contains a @nobody@ user but not a @foo@ user.

Let's look at how that works.

First, @main@ always runs in the IO monad only, so we take the result from
the later calls and put it in @rv@.  Note that the combined block
is started with @runErrorT $ do@ instead of just @do@.

To get something out of the call to 'readfile', we use
@join $ liftIO $ readfile@.  This will bring the result out of the IO monad
into the combined monad and process it like usual.  From here on,
everything looks normal, except for IO calls.  They are all executed under
@liftIO@ so that the result value is properly brought into the combined
monad.  This finally returns @\"done\"@.  Since we are in the Error monad, that means that the literal value is @Right \"done\"@.  Since we are also in the IO
monad, this is wrapped in IO.  So the final return type after applying
@runErrorT@ is @IO (Either CPError String)@.

In this case, there was an error, and processing stopped at that point just
like the example of the pure Error monad.  We print out the return value,
so you see the error displayed as a @Left@ value.

It all works quite easily.

-}

{- $configuringcp

You may notice that the 'ConfigParser' object has some configurable parameters,
such as 'usedefault'.  In case you're not familiar with the Haskell syntax
for working with these, you can use syntax like this to set these options:

>let cp2 = cp { usedefault = False }

This will create a new 'ConfigParser' that is the same as @cp@ except for
the 'usedefault' field, which is now always False.  The new object will be
called @cp2@ in this example.
-}

{- $reading

You can use these functions to read data from a file.

A common idiom for loading a new object from stratch is:

@cp <- 'readfile' 'emptyCP' \"\/etc\/foo.cfg\"@

Note the use of 'emptyCP'; this will essentially cause the file's data
to be merged with the empty 'ConfigParser'.
-}

{- $types

The code used to say this:

>type CPResult a = MonadError CPError m => m a
>simpleAccess :: ConfigParser -> SectionSpec -> OptionSpec -> CPResult String

But Hugs did not support that type declaration.  Therefore, types are now
given like this:

>simpleAccess :: MonadError CPError m =>
>                ConfigParser -> SectionSpec -> OptionSpec -> m String

Although it looks more confusing than before, it still means the same.
The return value can still be treated as @Either CPError String@ if you so
desire.
-}
