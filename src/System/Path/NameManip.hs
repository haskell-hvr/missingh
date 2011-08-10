{- |
   Module     : System.Path.NameManip
   Copyright  : Copyright (C) 2004 Volker Wysk
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Low-level path name manipulations.

Written by Volker Wysk
-}

module System.Path.NameManip where

import Data.List (intersperse)
import System.Directory (getCurrentDirectory)

{- | Split a path in components. Repeated \"@\/@\" characters don\'t lead to empty
components. \"@.@\" path components are removed. If the path is absolute, the first component
will start with \"@\/@\". \"@..@\" components are left intact. They can't be simply
removed, because the preceding component might be a symlink. In this case,
'realpath' is probably what you need.

The case that the path is empty, is probably an error. However, it is
treated like \"@.@\", yielding an empty path components list.

Examples:

>slice_path "/"        = ["/"]
>slice_path "/foo/bar" = ["/foo","bar"]
>slice_path "..//./"   = [".."]
>slice_path "."        = []

See 'unslice_path', 'realpath', 'realpath_s'.
-}
slice_path :: String    -- ^ The path to be broken to components.
           -> [String]  -- ^ List of path components.
slice_path p =
   case p of
      ('/':p') -> case slice_path' p' of
                     [] -> ["/"]
                     (c:cs) -> (('/':c):cs)
      _ -> slice_path' p
   where
      slice_path' o = filter (\c -> c /= "" && c /= ".") (split o)

      split ""      = []
      split ('/':o) = "" : split o
      split (x:xs)  = case split xs of
                         [] -> [[x]]
                         (y:ys) -> ((x:y):ys)

{- | Form a path from path components. This isn't the inverse
of 'slice_path', since @'unslice_path' . 'slice_path'@
normalises the path.

See 'slice_path'.
-}
unslice_path :: [String]        -- ^ List of path components
             -> String          -- ^ The path which consists of the supplied path components
unslice_path [] = "."
unslice_path cs = concat (intersperse "/" cs)


{- | Normalise a path. This is done by reducing repeated @\/@ characters to one, and removing
@.@ path components. @..@ path components are left intact, because of possible symlinks.

@'normalise_path' = 'unslice_path' . 'slice_path'@
-}
normalise_path :: String        -- ^ Path to be normalised
               -> String        -- ^ Path in normalised form
normalise_path = unslice_path . slice_path


{- | Split a file name in components. This are the base file name and the
suffixes, which are separated by dots. If the name starts with a dot, it is
regarded as part of the base name. The result is a list of file name
components. The filename may be a path. In this case, everything up to the
last path component will be returned as part of the base file name. The
path gets normalised thereby.

No empty suffixes are returned. If the file name contains several
consecutive dots, they are regared as part of the preceding file name
component.

Concateneting the name components and adding dots, reproduces the
original name, with a normalised path:
@concat . intersperse \".\" . 'slice_filename' == 'normalise'@.

Note that the last path component might be \"@..@\". Then it is not
possible to deduce the refered directory's name from the path. An IO
action for getting the real path is then necessary.

Examples:

@
'slice_filename' \"a.b\/\/.\/.foo.tar.gz\" == [\"a.b\/.foo\",\"tar\",\"gz\"]
'slice_filename' \".x..y.\"             == [\".x.\", \"y.\"]
@

See 'unslice_filename', @slice_filename\'@.
-}
slice_filename :: String        -- ^ Path
               -> [String]      -- ^ List of components the file name is made up of
slice_filename path =
   let comps = slice_path path
   in if comps == []
         then []
         else -- slice_filename' result not empty, because comps not empty
              let (base:suffixes) = slice_filename' (last comps)
              in (unslice_path (init comps ++ [base]) : suffixes)


{- | This is a variant of 'slice_filename'. It is like 'slice_filename', except for
being more efficient, and the filename must not contain any preceding path,
since this case isn't considered.

See 'slice_filename', 'unslice_filename'.
-}
slice_filename' :: String        -- ^ File name without path
                -> [String]      -- ^ List of components the file name is made up of
slice_filename' filename =
   case filename of
     ('.':filename') -> case slice_filename'' filename' of
                           []     -> ["."]
                           (t:ts) -> ('.':t) : ts
     filename -> slice_filename'' filename
   where
      slice_filename'' :: String -> [String]
      slice_filename'' "" = []
      slice_filename'' fn =
         let (beg,rest) = split1 fn
         in  (beg : slice_filename'' rest)

      split1 :: String -> (String, String)
      split1 (x:y:r) =
         if x == '.' && y /= '.' then ("", y:r)
                                 else let (beg,rest) = split1 (y:r)
                                      in  (x:beg,rest)
      split1 str = (str, "")



{- | Form file name from file name components, interspersing dots. This is
the inverse of 'slice_filename', except for normalisation of any path.

> unslice_filename = concat . intersperse "."

See 'slice_filename'.
-}
unslice_filename :: [String]    -- ^ List of file name components
                 -> String      -- ^ Name of the file which consists of the supplied components
unslice_filename = concat . intersperse "."


{- | Split a path in directory and file name. Only in the case that the
supplied path is empty, both parts are empty strings. Otherwise, @\".\"@ is filled in
for the corresponding part, if necessary. Unless the path is empty,
concatenating the returned path and file name components with a slash in
between, makes a valid path to the file.

@split_path@ splits off the last path component. This
isn't the same as the text after the last @\/@.

Note that the last path component might be @\"..\"@. Then it is not
possible to deduce the refered directory's name from the path. Then an IO
action for getting the real path is necessary.

Examples:

>split_path "/a/b/c"      == ("/a/b", "c")
>split_path "foo"         == (".", "foo")
>split_path "foo/bar"     == ("foo", "bar")
>split_path "foo/.."      == ("foo", "..")
>split_path "."           == (".", ".")
>split_path ""            == ("", "")
>split_path "/foo"        == ("/", "foo")
>split_path "foo/"        == (".", "foo")
>split_path "foo/."       == (".", "foo")
>split_path "foo///./bar" == ("foo", "bar")

See 'slice_path'.
-}
split_path :: String            -- ^ Path to be split
           -> (String, String)  -- ^ Directory and file name components of the path. The directory path is normalized.
split_path "" = ("","")
split_path path =
   case slice_path path of
      []      -> (".",".")
      ["/"]   -> ("/", ".")
      ['/':p] -> ("/", p)
      [fn]    -> (".", fn)
      parts   -> ( unslice_path (init parts)
                 , last parts
                 )

{- | Get the directory part of a path.

>dir_part = fst . split_path

See 'split_path'.
-}
dir_part :: String -> String
dir_part = fst . split_path


{- | Get the last path component of a path.

>filename_part = snd . split_path

Examples:

>filename_part "foo/bar" == "bar"
>filename_part "."       == "."

See 'split_path'.
-}
filename_part :: String -> String
filename_part = snd . split_path


{- | Inverse of 'split_path', except for normalisation.

This concatenates two paths, and takes care of @\".\"@ and empty paths. When the two components are the result of @split_path@, then @unsplit_path@
creates a normalised path. It is best documented by its definition:

>unsplit_path (".", "") = "."
>unsplit_path ("", ".") = "."
>unsplit_path (".", q)  = q
>unsplit_path ("", q)   = q
>unsplit_path (p, "")   = p
>unsplit_path (p, ".")  = p
>unsplit_path (p, q)    = p ++ "/" ++ q

Examples:

>unsplit_path ("", "")     == ""
>unsplit_path (".", "")    == "."
>unsplit_path (".", ".")   == "."
>unsplit_path ("foo", ".") == "foo"

See 'split_path'.
-}
unsplit_path :: ( String, String )  -- ^ Directory and file name
             -> String          -- ^ Path formed from the directory and file name parts
unsplit_path (".", "") = "."
unsplit_path ("", ".") = "."
unsplit_path (".", q)  = q
unsplit_path ("", q)   = q
unsplit_path (p, "")   = p
unsplit_path (p, ".")  = p
unsplit_path (p, q)    = p ++ "/" ++ q


{- | Split a file name in prefix and suffix. If there isn't any suffix in
the file name, then return an empty suffix. A dot at the beginning or at
the end is not regarded as introducing a suffix.

The last path component is what is being split. This isn't the same as
splitting the string at the last dot. For instance, if the file name
doesn't contain any dot, dots in previous path component's aren't mistaken
as introducing suffixes.

The path part is returned in normalised form. This means, @\".\"@ components
are removed, and multiple \"@\/@\"s are reduced to one.

Note that there isn't any plausibility check performed on the suffix. If the file name doesn't have a suffix, but happens to contain a dot, then this
dot is mistaken as introducing a suffix.

Examples:

>split_filename "path/to/foo.bar"                             = ("path/to/foo","bar")
>split_filename "path/to/foo"                                 = ("path/to/foo","")
>split_filename "/path.to/foo"                                = ("/path.to/foo","")
>split_filename "a///./x"                                     = ("a/x","")
>split_filename "dir.suffix/./"                               = ("dir","suffix")
>split_filename "Photographie, Das 20. Jahrhundert (300 dpi)" = ("Photographie, Das 20", " Jahrhundert (300 dpi)")

See 'slice_path', 'split_filename\''
-}
split_filename :: String                -- ^ Path including the file name to be split
               -> (String, String)      -- ^ The normalised path with the file prefix, and the file suffix.
split_filename "" = ("", "")
split_filename path =
   case slice_path path of
      []    -> (".","")
      comps -> let (pref_fn, suff_fn) = split_filename' (last comps)
               in ( concat (intersperse "/" (init comps ++ [pref_fn]))
                  , suff_fn
                  )


{- | Variant of 'split_filename'. This is a more efficient version
of 'split_filename', for the case that you know the string is
is a pure file name without any slashes.

See 'split_filename'.
-}
split_filename' :: String               -- ^ Filename to be split
                -> (String, String)     -- ^ Base name and the last suffix
split_filename' "" = ("", "")
split_filename' fn =
   let parts = slice_filename' fn
   in case parts of
         []     -> (".", "")
         [base] -> (base, "")
         p      -> (unslice_filename (init p), last p)


{- | Inverse of 'split_filename'. Concatenate prefix and suffix, adding
a dot in between, iff the suffix is not empty. The path part of the prefix is
normalised.

See 'split_filename'.
-}
unsplit_filename :: (String, String)    -- ^ File name prefix and suffix
                 -> String              -- ^ Path
unsplit_filename (prefix, suffix) =
   if suffix == "" then prefix else prefix ++ "." ++ suffix


{- | Split a path in directory, base file name and suffix.
-}
split3 :: String                        -- ^ Path to split
       -> (String, String, String)      -- ^ Directory part, base file name part and suffix part
split3 "" = ("","","")
split3 path =
   let comps = slice_path path
       (base, suffix) = split_filename' (last comps)
   in  (unslice_path (init comps), base, suffix)


{- |
Form path from directory, base file name and suffix parts.
-}
unsplit3 :: (String, String, String)    -- ^ Directory part, base file name part and suffix part
         -> String                      -- ^ Path consisting of dir, base and suffix
unsplit3 (dir, base, suffix) =
   unsplit_path (dir, (unsplit_filename (base,suffix)))


{- | Test a path for a specific suffix and split it off.

If the path ends with the suffix, then the result is @Just
prefix@, where @prefix@ is the normalised path
without the suffix. Otherwise it's @Nothing@.
-}
test_suffix :: String           -- ^ Suffix to split off
            -> String           -- ^ Path to test
            -> Maybe String     -- ^ Prefix without the suffix or @Nothing@
test_suffix suffix path =
    let (prefix, suff) = split_filename path
    in if suff == suffix then Just prefix
                         else Nothing


{- | Make a path absolute, using the current working directory.

This makes a relative path absolute with respect to the current
working directory. An absolute path is returned unmodified.

The current working directory is determined with @getCurrentDirectory@
which means that symbolic links in it are expanded and the path is
normalised. This is different from @pwd@.
-}
absolute_path :: String         -- ^ The path to be made absolute
              -> IO String      -- ^ Absulte path
absolute_path path@('/':_) = return path
absolute_path path = do
   cwd <- getCurrentDirectory
   return (cwd ++ "/" ++ path)


{- | Make a path absolute.

This makes a relative path absolute with respect to a specified
directory. An absolute path is returned unmodified.
-}
absolute_path_by :: String        -- ^ The directory relative to which the path is made absolute
                 -> String        -- ^ The path to be made absolute
                 -> String        -- ^ Absolute path
absolute_path_by _ path@('/':_) = path
absolute_path_by dir path = dir ++ "/" ++ path


{- | Make a path absolute.

This makes a relative path absolute with respect to a specified
directory. An absolute path is returned unmodified.

The order of the arguments can be confusing. You should rather use 'absolute_path_by'. @absolute_path\'@ is included for backwards compatibility.
-}
absolute_path' :: String        -- ^ The path to be made absolute
               -> String        -- ^ The directory relative to which the path is made absolute
               -> String        -- ^ Absolute path
absolute_path' path@('/':_) _ = path
absolute_path' path dir = dir ++ "/" ++ path


{- | Guess the @\"..\"@-component free form of a path, specified as a list of path components, by syntactically removing them, along with the preceding
   path components. This will produce
   erroneous results when the path contains symlinks. If the path contains leading @\"..\"@ components, or more @\"..\"@ components than preceeding normal
   components, then the @\"..\"@ components can't be normalised away. In this case, the result is @Nothing@.
-}
guess_dotdot_comps :: [String]          -- ^ List of path components
                   -> Maybe [String]    -- ^ In case the path could be transformed, the @\"..\"@-component free list of path components.
guess_dotdot_comps = guess_dotdot_comps' []
   where
      guess_dotdot_comps' schon [] = Just schon
      guess_dotdot_comps' [] ("..":_) = Nothing
      guess_dotdot_comps' schon ("..":teile) = guess_dotdot_comps' (reverse . tail . reverse $ schon) teile
      guess_dotdot_comps' schon (teil:teile) = guess_dotdot_comps' (schon ++ [teil]) teile


{- | Guess the @\"..\"@-component free, normalised form of a path. The transformation is purely syntactic. @\"..\"@ path components will be removed, along
   with their preceding path components. This will produce
   erroneous results when the path contains symlinks. If the path contains leading @\"..\"@ components, or more @\"..\"@ components than preceeding normal
   components, then the @\"..\"@ components can't be normalised away. In this case, the result is @Nothing@.

>guess_dotdot = fmap unslice_path . guess_dotdot_comps . slice_path
-}
guess_dotdot :: String                  -- ^ Path to be normalised
             -> Maybe String            -- ^ In case the path could be transformed, the normalised, @\"..\"@-component free form of the path.
guess_dotdot =
   fmap unslice_path . guess_dotdot_comps . slice_path
