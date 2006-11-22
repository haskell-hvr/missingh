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
   Module     : MissingH.StatusBar
   Copyright  : Copyright (C) 2006 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable

Tool for maintaining a status bar, supporting multiple simultaneous tasks,
as a layer atop "MissingH.ProgressTracker".

Written by John Goerzen, jgoerzen\@complete.org -}

module MissingH.StatusBar (
                          )

where
import MissingH.ProgressTracker
import Control.Concurrent.MVar
import MissingH.Str

data StatusBar = 
    StatusBar {masterP :: ProgressTracker,
               components :: [ProgressTracker],
               width :: Int,
               renderer :: Integer -> String}

type Status = MVar StatusBar

{- | Set up a new status bar. -}
newStatus :: ProgressTracker    -- ^ The top-level 'ProgressTracker'
          -> Int                -- ^ Width of the terminal -- usually 80
          -> (Integer -> String)-- ^ A function to render sizes
          -> IO Status
newStatus tracker w rfunc = 
    newMVar $ StatusBar {masterP = tracker, components = [],
                         width = w, renderer = rfunc}

{- | Render the current status. -}
renderStatus :: Status -> IO String
renderStatus r = withMVar r $ \status ->
    do overallpct <- renderpct (masterP status)
       components <- mapM rendercomponent (renderer status) (components status)
       overall <- renderoverall (renderer status) (masterP status)
    where renderpct pt = 
              withStatus pt renderpctpts
          renderpctpts pts = 
                  if (totalUnits pts == 0)
                     then return "0%"
                     else return $ show (((completedUnits pts) * 100) `div` (totalUnits pts)) ++ "%"
          rendercomponent rfunc pt = withStatus pt $ \pts ->
              return $ "[" ++ trackerName pt ++ " " ++
                     rfunc (completedUnits pts) ++ "/" ++
                     rfunc (totalUnits pts) ++ " " ++
                     renderpctpts pts ++ "]"
          renderoverall rfunc pt = withStatus pt $ \pts ->
              return $ " " ++ (renderer . getSpeed) pts ++ "/s " ++
                     

-- need to figure a way to render the timediff