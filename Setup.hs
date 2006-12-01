#!/usr/bin/env runhugs

import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Version
import System.Info
import Data.Maybe

winHooks = defaultUserHooks {confHook = customConfHook,
                             buildHook = customBuildHook}

customConfHook descrip flags =
    let mydescrip = case System.Info.os of
                      "mingw32" -> descrip
                      _ -> descrip {buildDepends = 
                                        (Dependency "unix" AnyVersion) :
                                        buildDepends descrip}
    in (confHook defaultUserHooks) mydescrip flags

customBuildHook descrip lbi uh flags =
    let myexecutables = map bdfix (executables descrip)
        bdfix exe = 
            exe {buildInfo = 
                     (buildInfo exe) 
                       {otherModules = 
                            exposedModules . fromJust . library $ descrip}}
        mydescrip = descrip {executables = myexecutables}
    in do print mydescrip
          (buildHook defaultUserHooks) mydescrip lbi uh flags
                                                          
main = defaultMainWithHooks winHooks

