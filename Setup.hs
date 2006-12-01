#!/usr/bin/env runhugs

import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Version
import System.Info
import Data.Maybe

missingHooks = defaultUserHooks {confHook = customConfHook}
                                }

customConfHook descrip flags =
    let mydescrip = case System.Info.os of
                      "mingw32" -> descrip
                      _ -> descrip {buildDepends = 
                                        (Dependency "unix" AnyVersion) :
                                        buildDepends descrip}
    in (confHook defaultUserHooks) mydescrip flags

main = defaultMainWithHooks missingHooks

