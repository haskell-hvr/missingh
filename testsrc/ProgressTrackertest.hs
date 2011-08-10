{- 
Copyright (C) 2006-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

module ProgressTrackertest(tests) where
import Data.Progress.Tracker
import Test.HUnit
import Control.Concurrent.MVar

setup =
    do timem <- newMVar 0
       let timesource = readMVar timem
       po <- newProgress' (ProgressStatus 0 100 0 "" timesource) []
       return (po, timem)

settime timem newval = swapMVar timem newval >> return ()

test_incrP = 
    do (po, timem) <- setup
       incrP po 5
       withStatus po $ \s ->
                  do assertEqual "completedUnits" 5 (completedUnits s)
                     assertEqual "totalUnits" 100 (totalUnits s)
       incrP po 95
       withStatus po $ \s ->
                  do assertEqual "completedUnits" 100 (completedUnits s)
                     assertEqual "totalUnits" 100 (totalUnits s)
       incrP po 5
       withStatus po $ \s ->
                  do assertEqual "completedUnits" 105 (completedUnits s)
                     assertEqual "totalUnits" 105 (totalUnits s)
       incrP' po 5
       withStatus po $ \s ->
                  do assertEqual "completedUnits" 110 (completedUnits s)
                     assertEqual "totalUnits" 105 (totalUnits s)
       incrTotal po 10
       withStatus po $ \s ->
                  do 110 @=? completedUnits s
                     115 @=? totalUnits s

test_setP =
    do (po, timem) <- setup
       setP po 5
       withStatus po $ \s ->
           do 5 @=? completedUnits s
              100 @=? totalUnits s
       setP po 100
       withStatus po $ \s ->
           do 100 @=? completedUnits s
              100 @=? totalUnits s
       setP po 105
       withStatus po $ \s ->
           do 105 @=? completedUnits s
              105 @=? totalUnits s
       setP' po 110
       withStatus po $ \s ->
           do 110 @=? completedUnits s
              105 @=? totalUnits s
       setTotal po 115
       withStatus po $ \s ->
           do 110 @=? completedUnits s
              115 @=? totalUnits s

test_speed =
    do (po, timem) <- setup
       getSpeed po >>= assertEqual "initial speed" 0
       getETR po >>= assertEqual "initial ETR" 0
       getETA po >>= assertEqual "initial ETA" 0

       incrP po 10
       getSpeed po >>= assertEqual "speed after incr" 0
       getETR po >>= assertEqual "ETR after incr" 0
       getETA po >>= assertEqual "ETA after incr" 0

       settime timem 5
       getSpeed po >>= assertEqual "first speed" 2.0
       getETR po >>= assertEqual "first ETR" 45
       getETA po >>= assertEqual "first ETA" 50

       incrP po 90
       getSpeed po >>= assertEqual "speed 2" 20.0
       getETR po >>= assertEqual "etr 2" 0
       getETA po >>= assertEqual "eta 2" 5

       settime timem 400
       setP po 90
       getSpeed po >>= assertEqual "speed 3" 0.225
       getETR po >>= assertEqual "etr 2" 44
       getETA po >>= assertEqual "eta 2" 444

test_callback =       
    do (po, _) <- setup
       mcounter <- newMVar (0::Int)
       mcounter1 <- newMVar (0::Int)
       mcounter2 <- newMVar (0::Int)
       (po2, _) <- setup
       (po3, _) <- setup
       
       addCallback po (minc mcounter)
       addParent po po2
       incrP po 5
       readMVar mcounter >>= assertEqual "cb1" 1
       withStatus po (\x -> 5 @=? completedUnits x)
       withStatus po2 (\x -> do 5 @=? completedUnits x
                                200 @=? totalUnits x)
       
       addCallback po2 (minc mcounter2)
       incrP po 100
       readMVar mcounter2 >>= (\x -> assertBool "cb2" (0 /= x))
       withStatus po2 (\x -> do 105 @=? completedUnits x
                                205 @=? totalUnits x)
       
       incrP' po 5
       withStatus po2 (\x -> do 110 @=? completedUnits x
                                205 @=? totalUnits x)

       finishP po
       withStatus po2 (\x -> do 110 @=? completedUnits x
                                210 @=? totalUnits x)
       
       
    where minc mv _ _ = modifyMVar_ mv (\x -> return $ x + 1)

tests = TestList [TestLabel "incrP" (TestCase test_incrP),
                  TestLabel "setP" (TestCase test_setP),
                  TestLabel "speed" (TestCase test_speed),
                  TestLabel "test_callback" (TestCase test_callback)]



