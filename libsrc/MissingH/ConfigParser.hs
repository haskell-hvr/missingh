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
-}
module MissingH.ConfigParser
    (
     -- * Types
     SectionSpec, OptionSpec, ConfigParser(..),
     CPError, CPResult,
     -- * Initialization
     -- $initialization
     empty,

     -- * Reading
     -- $reading
     readfile, readhandle, readstring,

     -- * Accessing Data
     get, getbool, getnum,

     -- * Setting Data
     set, setshow,

     -- * Meta-queries
     sections, has_section,
     options, has_option,
     items,

     -- * Miscellaneous Manipulation
     add_section, merge
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
        ConfigParser { content = plusFM (mapFM convFM (content src)) 
                                 (content dest),
                       optionxform = optionxform dest,
                       usedefault = usedefault dest,
                       defaulthandler = defaulthandler dest,
                       accessfunc = accessfunc dest}

{- | Utility to do a special case merge. -}
readutil :: ConfigParser -> ParseOutput -> ConfigParser
readutil old new = 
    let mergedest = ConfigParser {content = fromAL new,
                                  optionxform = optionxform old,
                                  usedefault = usedefault old,
                                  defaulthandler = defaulthandler old,
                                  accessfunc = accessfunc old}
        in
        merge old mergedest

{- | Loads data from the specified file.  It is then combined with the
given 'ConfigParser' using the semantics documented under 'merge' with the
new data taking precedence over the old.  However, unlike
'merge', all the options
as set in the old object are preserved since the on-disk representation
does not convey those options.

May raise an exception on a syntax error or if the file could not be
accessed.
-}
readfile :: ConfigParser -> FilePath -> IO (CPResult ConfigParser)
readfile cp fp = do n <- parse_file fp
                    return $ readutil cp n

{- | Like 'readfile', but uses an already-open handle.  You should
use 'readfile' instead of this if possible, since it will be able to
generate better error messages.

May raise an exception on a syntax error.
-}
readhandle :: ConfigParser -> Handle -> IO (CPResult ConfigParser)
readhandle cp h = do n <- parse_handle h
                     return $ readutil cp n

{- | Like 'readfile', but uses a string.  You should use 'readfile'
instead of this if you are processing a file, since it can generate
better error messages.

May raise an exception on a syntax error.
-}
readstring :: ConfigParser -> String -> CPResult ConfigParser
readstring cp s = readutil cp $ parse_string s

{- | Returns a list of sections in your configuration file.  Never includes
the always-present section @DEFAULT@. -}
sections :: ConfigParser -> [SectionSpec]
sections = filter (/= "DEFAULT") . keysFM . content

{- | Indicates whether the given section exists.

No special @DEFAULT@ processing is done. -}
has_section :: ConfigParser -> SectionSpec -> Bool
has_section cp x = elemFM x (content cp)

{- | Adds the specified section name.  Raises an exception if the
section was already present.  Otherwise, returns the new 
'ConfigParser' object.-}
add_section :: ConfigParser -> SectionSpec -> CPResult ConfigParser
add_section cp s =
    if has_section cp s
       then throwError $ SectionAlreadyExists ("add_section: section " ++ s ++ " already exists")
       else return $ cp {content = addToFM (content cp) s emptyFM}

{- | Returns a list of the names of all the options present in the
given section.

Returns an error if the given section does not exist.
-}
options :: ConfigParser -> SectionSpec -> CPResult [OptionSpec]
options cp x = maybeToEither (NoSection x) $ 
               do
               o <- lookupFM (content cp) x
               return $ keysFM o

{- | Indicates whether the given option is present.  Returns True
only if the given section is present AND the given option is present
in that section.  No special @DEFAULT@ processing is done.  No
exception could be raised.
-}
has_option :: ConfigParser -> SectionSpec -> OptionSpec -> Bool
has_option cp s o = 
    let c = content cp
        v = do secthash <- lookupFM c s
               return $ elemFM (optionxform cp $ o) secthash
        in
        case v of
               Nothing -> False
               Just x -> x
                           
{- | Retrieves a string from the configuration file.

Returns an error if no such section/option could be found.
-}
get :: ConfigParser -> SectionSpec -> OptionSpec -> CPResult String
get cp = (accessfunc cp) cp

{- | Retrieves a string from the configuration file and attempts to parse it
as a number.  Raises an exception if no such option could be found or if it
could not be parsed as the destination number. -}
getnum :: (Read a, Num a) => ConfigParser -> SectionSpec -> OptionSpec -> CPResult a
getnum cp s o = get cp s o >>= return . read

{- | Retrieves a string from the configuration file and attempts to parse
it as a boolean.  

Returns an error if no such option could be found or
if it could not be parsed as a boolean. -}
getbool :: ConfigParser -> SectionSpec -> OptionSpec -> CPResult Bool
getbool cp s o = 
    do val <- get cp s o
       case map toLower . strip $ val of
                  "1" -> return True
                  "yes" -> return True
                  "on" -> return True
                  "enabled" -> return True
                  "0" -> return False
                  "no" -> return False
                  "off" -> return False
                  "disabled" -> return False
                  _ -> throwError (ParseError $ "getbool: couldn't parse " ++
                                   val ++ " from " ++ s ++ "/" ++ o)

{- | Returns a list of @(optionname, value)@ pairs representing the content
of the given section.  Returns an error the section is invalid. -}
items :: ConfigParser -> SectionSpec -> CPResult [(OptionSpec, String)]
items cp s = do fm <- maybeToEither (NoSection s) $ lookupFM (content cp) s
                return $ fmToList fm

{- | Sets the option to a new value, replacing an existing one if it exists.

Returns an error if the section does not exist. -}
set :: ConfigParser -> SectionSpec -> OptionSpec -> String -> CPResult ConfigParser
set cp s passedo val = 
    do sectmap <- maybeToEither (NoSection s) $ lookupFM (content cp) s
       let o = (optionxform cp) passedo
       let newsect = addToFM sectmap o val
       let newmap = addToFM (content cp) s newsect
       return $ cp { content = newmap}

{- | Sets the option to a new value, replacing an existing one if it exists.
It requires only a showable value as its parameter.
This can be used with bool values, as well as numeric ones.

Returns an error if the section does not exist. -}
setshow :: Show a => ConfigParser -> SectionSpec -> OptionSpec -> a -> CPResult ConfigParser
setshow cp s o val = set cp s o (show val)

{- | Converts the 'ConfigParser' to a string representation that could be
later re-parsed by this module. -}
to_string :: ConfigParser -> String
to_string cp = 
    let gen_option (key, value) = 
            key ++ ": " ++ (replace "\n" "\n " value) ++ "\n"
        gen_section (sect, valfm) = 
            "[" ++ sect ++ "]\n" ++
                (concat $ map gen_option (fmToList valfm)) ++ "\n"
        in
        concat $ map gen_section (fmToList (content cp))

----------------------------------------------------------------------
-- Docs
----------------------------------------------------------------------

{- $initialization

The variable 'empty' is exported, and contains a default empty
'ConfigParser'.
-}

{- $reading

You can use these functions to read data from a file.

A common idiom for loading a new object from stratch is:

@cp <- 'readfile' 'empty' \"\/etc\/foo.cfg\"@

Note the use of 'empty'; this will essentially cause the file's data
to be merged with the empty 'ConfigParser'.
-}