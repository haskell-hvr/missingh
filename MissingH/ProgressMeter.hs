{-
Copyright (C) 2006 John Goerzen <jgoerzen@complete.org>

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
   Module     : MissingH.ProgressMeter
   Copyright  : Copyright (C) 2006 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable

Tool for maintaining a status bar, supporting multiple simultaneous tasks,
as a layer atop "MissingH.ProgressTracker".

Written by John Goerzen, jgoerzen\@complete.org -}

module MissingH.ProgressMeter 


where
import MissingH.ProgressTracker
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad(when)
import MissingH.Str
import MissingH.Time
import MissingH.Quantity

data ProgressMeterR = 
    ProgressMeterR {masterP :: Progress,
                    components :: [Progress],
                    width :: Int,
                    renderer :: Integer -> String,
                    autoDisplayers :: [ThreadId]}

type ProgressMeter = MVar ProgressMeterR

{- | Set up a new status bar using defaults:

* The given tracker

* Width 80

* MissingH.Quantity.renderNum binaryOpts 0

-}
simpleNewMeter :: Progress -> IO ProgressMeter
simpleNewMeter pt = newMeter pt 80 (renderNum binaryOpts 0)

{- | Set up a new status bar. -}
newMeter :: Progress           -- ^ The top-level 'Progress'
          -> Int                -- ^ Width of the terminal -- usually 80
          -> (Integer -> String)-- ^ A function to render sizes
          -> IO ProgressMeter
newMeter tracker w rfunc = 
    newMVar $ ProgressMeterR {masterP = tracker, components = [],
                         width = w, renderer = rfunc, autoDisplayers = []}

{- | Adjust the list of components of this 'ProgressMeter'. -}
setComponents :: ProgressMeter -> [Progress] -> IO ()
setComponents meter componentlist = modifyMVar_ meter (\m -> return $ m {components = componentlist})

{- | Adjusts the width of this 'ProgressMeter'. -}
setWidth :: ProgressMeter -> Int -> IO ()
setWidth meter w = modifyMVar_ meter (\m -> return $ m {width = w})

{- | Like renderMeter, but prints it to the screen instead of returning it. 

This function will output CR, then the meter. -}
displayMeter :: ProgressMeter -> IO ()
displayMeter r = 
    do s <- renderMeter r
       putStr ("\r" ++ s)

{- | Clears the meter -- outputs CR, spaces equal to the width - 1,
then another CR. -}
clearMeter :: ProgressMeter -> IO ()
clearMeter pm = withMVar pm $ \m ->
                putStr $ "\r" ++ replicate (width m - 1) " " ++ "\r"

{- | Starts a thread that updates the meter every n seconds by calling
the specified function.  Note: 'displayMeter' is an ideal function here.

Save this threadID and use it later to call 'stopAutoDisplayMeter'. -}
autoDisplayMeter :: ProgressMeter -> Int -> (ProgressMeter -> IO ()) -> IO ThreadId
autoDisplayMeter pm delay displayfunc =
    do thread <- forkIO workerthread
       modifyMVar_ pm (\p -> return $ p {autoDisplayers = thread : autoDisplayers p})
       return thread
    where workerthread = do tid <- myThreadId
                            -- Help fix a race condition so that the above
                            -- modifyMVar can run before a check ever does
                            yield
                            loop tid
          loop tid = do displayMeter pm
                        threadDelay (delay * 1000000)
                        c <- doIContinue tid
                        when c (loop tid)
          doIContinue tid = withMVar pm $ \p ->
                               if tid `elem` autoDisplayers p
                                  then return True
                                  else return False

{- | Stops the specified meter from displaying.

You should probably call 'clearMeter' after a call to this. -}
killAutoDisplayMeter :: ProgressMeter -> ThreadId -> IO ()
killAutoDisplayMeter pm t = 
    modifyMVar_ pm (\p -> return $ p {autoDisplayers = filter (/= t) (autoDisplayers p)})

{- | Render the current status. -}
renderMeter :: ProgressMeter -> IO String
renderMeter r = withMVar r $ \meter ->
    do overallpct <- renderpct (masterP meter)
       components <- mapM (rendercomponent (renderer meter))
                     (components meter)
       let componentstr = case join " " components of
                            [] -> ""
                            x -> x ++ " "
       rightpart <- renderoverall (renderer meter) (masterP meter)
       let leftpart = overallpct ++ componentstr
       let padwidth = (width meter) - 1 - (length leftpart) - (length rightpart)
       if padwidth < 1
          then return $ take (width meter - 1) $ leftpart ++ rightpart
          else return $ leftpart ++ replicate padwidth ' ' ++ rightpart
       
    where renderpct pt = 
              withStatus pt renderpctpts
          renderpctpts pts = 
                  if (totalUnits pts == 0)
                     then return "0% "
                     else return $ show (((completedUnits pts) * 100) `div` (totalUnits pts)) ++ "% "
          rendercomponent :: (Integer -> String) -> Progress -> IO String
          rendercomponent rfunc pt = withStatus pt $ \pts ->
              do pct <- renderpctpts pts
                 return $ "[" ++ trackerName pts ++ " " ++
                     rfunc (completedUnits pts) ++ "B/" ++
                     rfunc (totalUnits pts) ++ "B " ++ pct ++ "]"
          renderoverall rfunc pt = withStatus pt $ \pts ->
              do etr <- getETR pts
                 speed <- getSpeed pts
                 return $ rfunc (floor speed) ++ "B/s " ++ renderSecs etr


