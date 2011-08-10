{-# LANGUAGE CPP #-}
{- Platform Compatibility Layer
Copyright (c) 2005-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : System.IO.PlafCompat
   Copyright  : Copyright (C) 2005-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable

On Unix, exports System.Posix.Types and System.Posix.Files.

On Windows, exports System.Posix.Types and "System.IO.WindowsCompat".

The result should be roughly the same set of defined variables and types.

-}

module System.IO.PlafCompat
    (nullFileName,
#if (defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
     module System.IO.WindowsCompat,
#else
     module System.Posix.Files,
#endif
     module System.Posix.Types)
where

import System.Posix.Types
#if (defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
import System.IO.WindowsCompat
#else
import System.Posix.Files
#endif

{- | The name of the null device.  NUL: on Windows, \/dev\/null everywhere else.
-}

nullFileName :: String
#if (defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
nullFileName = "NUL:"
#else
nullFileName = "/dev/null"
#endif
