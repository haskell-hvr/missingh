{- arch-tag: MIME Types main file
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
   Module     : MissingH.MIMETypes
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Utilities for guessing MIME types of files.

Written by John Goerzen, jgoerzen\@complete.org
-}

module MissingH.MIMETypes (-- * Creating Lookup Objects
                           defaultmtd,
                           readMIMETypes,
                           hReadMIMETypes,
                           readSystemMIMETypes,
                           -- * Basic Access
                           MIMEResults,
                           MIMETypeData(..),
                           guessType,
                           guessExtension,
                           guessAllExtensions
                          )
where

import Data.FiniteMap
import qualified System.Directory
import Monad
import System.IO
import System.IO.Error
import MissingH.IO
import MissingH.Path
import MissingH.FiniteMap
import Data.Char

----------------------------------------------------------------------
-- Basic type decl
----------------------------------------------------------------------

data MIMETypeData = MIMETypeData 
    {
     -- | A mapping used to expand common suffixes into equivolent,
     -- better-parsed versions.  For instance, ".tgz" would expand
     -- into ".tar.gz".
     suffixMap :: FiniteMap String String,
     -- | A mapping used to determine the encoding of a file.
     -- This is used, for instance, to map ".gz" to "gzip".
     encodingsMap :: FiniteMap String String,
     -- | A mapping used to map extensions to MIME types.
     typesMap :: FiniteMap String String,
     -- | A mapping used to augment the 'typesMap' when non-strict
     -- lookups are used.
     commonTypesMap :: FiniteMap String String
    }

{- | Return value from guessing a file's type.

The first element of the tuple gives the MIME type.  It is Nothing if no
suitable type could be found.

The second element gives the encoding.  It is Nothing if there was no particular
encoding for the file, or if no encoding could be found.
-}
type MIMEResults = (Maybe String,       -- The MIME type
                    Maybe String        -- Encoding
                   )

{- | Read the given mime.types file and add it to an existing object.
Returns new object. -}


readMIMETypes :: MIMETypeData            -- ^ Data to work with
              -> Bool                    -- ^ Whether to work on strict data
              -> FilePath               -- ^ File to read
              -> IO MIMETypeData           -- ^ New object
readMIMETypes mtd strict fn = do
                         h <- openFile fn ReadMode
                         retval <- hReadMIMETypes mtd strict h
                         return retval

{- | Load a mime.types file from an already-open handle. -}
hReadMIMETypes :: MIMETypeData          -- ^ Data to work with
                  -> Bool               -- ^ Whether to work on strict data
                  -> Handle             -- ^ Handle to read from
                  -> IO MIMETypeData       -- ^ New object
hReadMIMETypes mtd strict h = 
    let parseline :: MIMETypeData -> String -> MIMETypeData
        parseline obj line =
            let l1 = words line 
                procwords [] = []
                procwords (('#':_) :_) = []
                procwords (x:xs) = x : procwords xs
                l2 = procwords l1
                in
                if (length l2) >= 2 then
                   let thetype = head l2
                       suffixlist = tail l2
                       in
                       foldl (\o suff -> addType o strict thetype ('.' : suff)) obj suffixlist
                else obj
        in
        do
        lines <- hGetLines h
        return (foldl parseline mtd lines)

{- | Guess the type of a file given a filename or URL.  The file
   is not opened; only the name is considered. -}

guessType :: MIMETypeData               -- ^ Source data for guessing
             -> Bool                    -- ^ Whether to limit to strict data
             -> String                  -- ^ File or URL name to consider
             -> MIMEResults             -- ^ Result of guessing (see 'MIMEResults' for details on interpreting it)
guessType mtd strict fn = 
    let mapext (base, ext) =
            case lookupFM (suffixMap mtd) ext of
                Nothing -> (base, ext)
                Just x -> mapext (splitExt (base ++ x))
        checkencodings (base, ext) =
            case lookupFM (encodingsMap mtd) ext of
                 Nothing -> (base, ext, Nothing)
                 Just x -> (fst (splitExt base),
                            snd (splitExt base),
                            Just x)
        (base, ext, enc) = checkencodings . mapext $ splitExt fn
        typemap = getStrict mtd strict
        in
        case lookupFM typemap ext of
             Nothing -> (lookupFM typemap (map toLower ext), enc)
             Just x -> (Just x, enc)

{- | Guess the extension of a file based on its MIME type.
   The return value includes the leading dot.

   Returns Nothing if no extension could be found.

   In the event that multiple possible extensions are available,
   one of them will be picked and returned.  The logic to select one
   of these should be considered undefined. -}
guessExtension :: MIMETypeData          -- ^ Source data for guessing
                  -> Bool               -- ^ Whether to limit to strict data
                  -> String             -- ^ MIME type to consider
                  -> Maybe String       -- ^ Result of guessing, or Nothing if no match possible
guessExtension mtd strict fn = 
    case guessAllExtensions mtd strict fn of
                                          [] -> Nothing
                                          (x:_) -> Just x

{- | Similar to 'guessExtension', but returns a list of all possible matching
extensions, or the empty list if there are no matches. -}
guessAllExtensions :: MIMETypeData      -- ^ Source data for guessing
                      -> Bool           -- ^ Whether to limit to strict data
                      -> String         -- ^ MIME type to consider
                      -> [String]       -- ^ Result of guessing
guessAllExtensions mtd strict fn =
    let mimetype = map toLower fn
        themap = getStrict mtd strict
        in
        flippedLookupFM themap mimetype
        
{- | Adds a new type to the data structures, replacing whatever data
   may exist about it already.  That is, it overrides existing information
   about the given extension, but the same type may occur more than once. -}

addType :: MIMETypeData                 -- ^ Source data
           -> Bool                      -- ^ Whether to add to strict data set
           -> String                    -- ^ MIME type to add
           -> String                    -- ^ Extension to add
           -> MIMETypeData              -- ^ Result of addition
addType mtd strict thetype theext = 
    setStrict mtd strict (\m -> addToFM m theext thetype)

{- | Default MIME type data to use -}
defaultmtd :: MIMETypeData
defaultmtd = 
    MIMETypeData {suffixMap = default_suffix_map,
                  encodingsMap = default_encodings_map,
                  typesMap = default_types_map,
                  commonTypesMap = default_common_types}

{- | Read the system's default mime.types files, and add the data contained
therein to the passed object, then return the new one. -}
readSystemMIMETypes :: MIMETypeData -> IO MIMETypeData
readSystemMIMETypes mtd =
    let tryread :: MIMETypeData -> String -> IO MIMETypeData
        tryread inputobj filename = 
            do
            fn <- try (openFile filename ReadMode)
            case fn of
                    Left _ -> return inputobj
                    Right h -> do
                               x <- hReadMIMETypes inputobj True h
                               hClose h
                               return x
        in
        do
        foldM tryread mtd defaultfilelocations

----------------------------------------------------------------------
-- Internal utilities
----------------------------------------------------------------------

getStrict :: MIMETypeData -> Bool -> FiniteMap String String
getStrict mtd True = typesMap mtd
getStrict mtd False = plusFM (commonTypesMap mtd) (typesMap mtd)

setStrict :: MIMETypeData -> Bool -> (FiniteMap String String -> FiniteMap String String) -> MIMETypeData
setStrict mtd True func = mtd{typesMap = func (typesMap mtd)}
setStrict mtd False func = mtd{commonTypesMap = func (commonTypesMap mtd)}

----------------------------------------------------------------------
-- Default data structures
----------------------------------------------------------------------

defaultfilelocations =
    [
     "/etc/mime.types",
     "/usr/local/etc/httpd/conf/mime.types",
     "/usr/local/lib/netscape/mime.types",
     "/usr/local/etc/httpd/conf/mime.types",     -- Apache 1.2
     "/usr/local/etc/mime.types"                -- Apache 1.3
    ]


default_encodings_map = listToFM [
                                  (".Z", "compress"),
                                  (".gz", "gzip"),
                                  (".bz2", "bzip2")
                                 ]
                               
default_suffix_map = listToFM [
                               (".tgz", ".tar.gz"),
                               (".tz", ".tar.gz"),
                               (".taz", ".tar.gz")
                               ]

default_types_map = listToFM [
                              (".a", "application/octet-stream"),
                              (".ai", "application/postscript"),
                              (".aif", "audio/x-aiff"),
                              (".aifc", "audio/x-aiff"),
                              (".aiff", "audio/x-aiff"),
                              (".au", "audio/basic"),
                              (".avi", "video/x-msvideo"),
                              (".bat", "text/plain"),
                              (".bcpio", "application/x-bcpio"),
                              (".bin", "application/octet-stream"),
                              (".bmp", "image/x-ms-bmp"),
                              (".c", "text/plain"),
                              (".cdf", "application/x-netcdf"),
                              (".cpio", "application/x-cpio"),
                              (".csh", "application/x-csh"),
                              (".css", "text/css"),
                              (".dll", "application/octet-stream"),
                              (".doc", "application/msword"),
                              (".dot", "application/msword"),
                              (".dvi", "application/x-dvi"),
                              (".eml", "message/rfc822"),
                              (".eps", "application/postscript"),
                              (".etx", "text/x-setext"),
                              (".exe", "application/octet-stream"),
                              (".gif", "image/gif"),
                              (".gtar", "application/x-gtar"),
                              (".h", "text/plain"),
                              (".hdf", "application/x-hdf"),
                              (".htm", "text/html"),
                              (".html", "text/html"),
                              (".ief", "image/ief"),
                              (".jpe", "image/jpeg"),
                              (".jpeg", "image/jpeg"),
                              (".jpg", "image/jpeg"),
                              (".js", "application/x-javascript"),
                              (".ksh", "text/plain"),
                              (".latex", "application/x-latex"),
                              (".m1v", "video/mpeg"),
                              (".man", "application/x-troff-man"),
                              (".me", "application/x-troff-me"),
                              (".mht", "message/rfc822"),
                              (".mhtml", "message/rfc822"),
                              (".mif", "application/x-mif"),
                              (".mov", "video/quicktime"),
                              (".movie", "video/x-sgi-movie"),
                              (".mp2", "audio/mpeg"),
                              (".mp3", "audio/mpeg"),
                              (".mpa", "video/mpeg"),
                              (".mpe", "video/mpeg"),
                              (".mpeg", "video/mpeg"),
                              (".mpg", "video/mpeg"),
                              (".ms", "application/x-troff-ms"),
                              (".nc", "application/x-netcdf"),
                              (".nws", "message/rfc822"),
                              (".o", "application/octet-stream"),
                              (".obj", "application/octet-stream"),
                              (".oda", "application/oda"),
                              (".p12", "application/x-pkcs12"),
                              (".p7c", "application/pkcs7-mime"),
                              (".pbm", "image/x-portable-bitmap"),
                              (".pdf", "application/pdf"),
                              (".pfx", "application/x-pkcs12"),
                              (".pgm", "image/x-portable-graymap"),
                              (".pl", "text/plain"),
                              (".png", "image/png"),
                              (".pnm", "image/x-portable-anymap"),
                              (".pot", "application/vnd.ms-powerpoint"),
                              (".ppa", "application/vnd.ms-powerpoint"),
                              (".ppm", "image/x-portable-pixmap"),
                              (".pps", "application/vnd.ms-powerpoint"),
                              (".ppt", "application/vnd.ms-powerpoint"),
                              (".ps", "application/postscript"),
                              (".pwz", "application/vnd.ms-powerpoint"),
                              (".py", "text/x-python"),
                              (".pyc", "application/x-python-code"),
                              (".pyo", "application/x-python-code"),
                              (".qt", "video/quicktime"),
                              (".ra", "audio/x-pn-realaudio"),
                              (".ram", "application/x-pn-realaudio"),
                              (".ras", "image/x-cmu-raster"),
                              (".rdf", "application/xml"),
                              (".rgb", "image/x-rgb"),
                              (".roff", "application/x-troff"),
                              (".rtx", "text/richtext"),
                              (".sgm", "text/x-sgml"),
                              (".sgml", "text/x-sgml"),
                              (".sh", "application/x-sh"),
                              (".shar", "application/x-shar"),
                              (".snd", "audio/basic"),
                              (".so", "application/octet-stream"),
                              (".src", "application/x-wais-source"),
                              (".sv4cpio", "application/x-sv4cpio"),
                              (".sv4crc", "application/x-sv4crc"),
                              (".swf", "application/x-shockwave-flash"),
                              (".t", "application/x-troff"),
                              (".tar", "application/x-tar"),
                              (".tcl", "application/x-tcl"),
                              (".tex", "application/x-tex"),
                              (".texi", "application/x-texinfo"),
                              (".texinfo", "application/x-texinfo"),
                              (".tif", "image/tiff"),
                              (".tiff", "image/tiff"),
                              (".tr", "application/x-troff"),
                              (".tsv", "text/tab-separated-values"),
                              (".txt", "text/plain"),
                              (".ustar", "application/x-ustar"),
                              (".vcf", "text/x-vcard"),
                              (".wav", "audio/x-wav"),
                              (".wiz", "application/msword"),
                              (".xbm", "image/x-xbitmap"),
                              (".xlb", "application/vnd.ms-excel"),
                              (".xls", "application/vnd.ms-excel"),
                              (".xml", "text/xml"),
                              (".xpm", "image/x-xpixmap"),
                              (".xsl", "application/xml"),
                              (".xwd", "image/x-xwindowdump"),
                              (".zip", "application/zip")
                             ]

default_common_types = listToFM [
                                 (".jpg", "image/jpg"),
                                 (".mid", "audio/midi"),
                                 (".midi", "audio/midi"),
                                 (".pct", "image/pict"),
                                 (".pic", "image/pict"),
                                 (".pict", "image/pict"),
                                 (".rtf", "application/rtf"),
                                 (".xul", "text/xul")
                                ]
