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
                           newMIMETypes,
                           readSystemMIMETypes,
                           -- * Basic Access
                           MIMEResults,
                           MIMETypeFunctions(..),
                           -- * Advanced Usage
                           MIMETypeData(..),
                           makeMIMETypes,
                          )
where

import Data.FiniteMap
import qualified System.Directory
import System.IO
import System.IO.Error
import MissingH.IO

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

data MIMETypeFunctions = MIMETypeFunctions
    {
     {- | Read the given mime.types file.  The first argument is whether or not to add to the strict table. -}
     readMimeTypes ::  Bool -> FilePath -> IO MIMETypeFunctions,
     {- | Load a mime.types file from an already-open handle. 
       The first argument is whether or not to add to the strict table. -}
     hReadMimeTypes :: Bool -> Handle -> IO MIMETypeFunctions,
     {- | Guess the type of a file given a filename or URL.  The file
     is not opened; only the name is considered.

     The first argument says whether or not to use strict mode.  The second
     gives the filename or URL.-}
     guessType :: Bool -> String -> MIMEResults,
     {- | Guess the extension of a file based on its MIME type.
     The return value includes the leading dot.  The first parameter
     is whether or not to use strict mode; the second is the MIME type.
     Returns Nothing if no extension could be found. -}
     guessExtension :: Bool -> String -> Maybe String,
     {- | Adds a new type to the data structures, replacing whatever data
     may exist about it already.  The first parameter denotes whether
     or not to add to the strict structures.  The second gives the MIME type,
     and the third gives the extension. -}
     addType :: Bool -> String -> String -> MIMETypeFunctions,
     {- | Advanced users: returns the internal 'MIMETypeData'. -}
     getMIMETypeData :: MIMETypeData,
     {- | Advanced users: sets the internal 'MIMETypeData',
     returning a new object with it. -}
     setMIMETypeData :: MIMETypeData -> MIMETypeFunctions
    }     

defaultMIMETypeData :: MIMETypeData
defaultMIMETypeData = 
    MIMETypeData {suffixMap = default_suffix_map,
                  encodingsMap = default_encodings_map,
                  typesMap = default_types_map,
                  commonTypesMap = default_common_types}

{- | Create a new MIME type lookup object.  This is the main entry
into the library. -}
newMIMETypes :: MIMETypeFunctions
newMIMETypes = makeMIMETypes defaultMIMETypeData

{- | Create a new MIME type lookup object based on the given
'MIMETypeData' object. -}
makeMIMETypes :: MIMETypeData -> MIMETypeFunctions
makeMIMETypes mtd = 
    let self = makeMIMETypes mtd in
    -- FIXME
    let hrmt strict h = 
            let parseline :: MIMETypeFunctions -> String -> MIMETypeFunctions
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
                               foldl (\o suff -> (addType o) strict thetype suff) obj suffixlist
                        else self
                in
                do
                lines <- hGetLines h
                return (foldl parseline self lines)

        in
        MIMETypeFunctions 
        {
         readMimeTypes = (\strict fn -> (do
                                         h <- openFile fn ReadMode
                                         retval <- hrmt strict h
                                         hClose h
                                         return retval
                                        )
                         ),
         hReadMimeTypes = hrmt,
         -- FIXME
         guessType = \strict thefile -> (Nothing, Nothing),
         -- FIXME
         guessExtension = \strict thetype -> Nothing,
         -- FIXME
         addType = \strict thetype theext -> makeMIMETypes mtd,
         getMIMETypeData = mtd,
         setMIMETypeData = \newmtd -> makeMIMETypes newmtd
        }


{- | Read the system's default mime.types files, and add the data contained
therein to the passed object, then return the new one. -}
readSystemMIMETypes :: MIMETypeFunctions -> IO MIMETypeFunctions
readSystemMIMETypes mtf =
    let tryread :: IO MIMETypeFunctions -> String -> IO MIMETypeFunctions
        tryread inputio filename = 
            do
            inputobj <- inputio
            fn <- try (openFile filename ReadMode)
            case fn of
                    Left _ -> return inputobj
                    Right h -> do
                               x <- (hReadMimeTypes inputobj) True h
                               hClose h
                               return x
        in
        do
        foldl tryread (return mtf) defaultfilelocations

{- | Advanced users: create a new MIME type lookup object based on a
'MIMETypeData' object.
-}

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
