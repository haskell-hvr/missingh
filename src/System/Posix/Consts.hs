{-# LANGUAGE CPP #-}
{- Posix consts not included with Haskell
Copyright (c) 2005-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : System.Posix.Consts
   Copyright  : Copyright (C) 2005-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable

Exports some POSIX constants and functions that are not exported in fptools
by default.

-}

module System.Posix.Consts where
import System.Posix.Types

blockSpecialMode :: FileMode
blockSpecialMode = 0o0060000

characterSpecialMode :: FileMode
characterSpecialMode = 0o0020000

namedPipeMode :: FileMode
namedPipeMode = 0o0010000

regularFileMode :: FileMode
regularFileMode = 0o0100000

directoryMode :: FileMode
directoryMode = 0o0040000

fileTypeModes :: FileMode
fileTypeModes = 0o00170000

socketMode :: FileMode
socketMode = 0o0140000

symbolicLinkMode :: FileMode
symbolicLinkMode = 0o0120000


