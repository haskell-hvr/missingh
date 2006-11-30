#!/usr/bin/env runhugs

import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Version
import System.Info

winHooks = defaultUserHooks {readDesc = customReadDesc}

customReadDesc =
    do pdesc <- readDesc defaultUserHooks
       case pdesc of
         Nothing -> return Nothing
         Just d -> if System.Info.os == "mingw32"
                      then return pdesc
                      else return $ Just (d {buildDepends = 
                                               (Dependency "unix" AnyVersion) :
                                               buildDepends d})

                                                          
main = defaultMainWithHooks winHooks

