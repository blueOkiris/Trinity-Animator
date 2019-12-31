module DrawPaneClick where

import Graphics.Gloss.Interface.Pure.Game(Event(..), SpecialKey(..), KeyState(..), Key(..), MouseButton(..))

import State    ( AppWindow(..), AppVector(..), AppState(..), DrawTool
                , chaikinOpen, vectorClosestToPoint
                , newDrawing, isMakingNewDrawing, moveDrawing, isMovingDrawing, noState, editDrawing, isEditingDrawing )
import GUI(Element(..), DynamicElement(..))
import DrawElement(getX1, getX2, getY1, getY2)

-- Allow for drawing to plane
drawPaneHandler :: Event -> AppState -> (DynamicElement AppState) -> Int -> AppState
drawPaneHandler (EventKey (MouseButton btn) upOrDown modifier (x, y)) state elem index =
    -- First check to make sure we're actually in the element
    if x >= fromIntegral (getX1 (elemCore elem) state) && x <= fromIntegral (getX2 (elemCore elem) state) 
        && y >= fromIntegral (getY1 (elemCore elem) state) && y <=  fromIntegral (getY2 (elemCore elem) state) then
            if btn == LeftButton then
                if upOrDown == Down && ((drawTool state) == newDrawing || (drawTool state) == isMakingNewDrawing) then
                    --trace "Down!"
                    state   { drawTool = changeToolDown 
                            , currentDrawing = AppVector { pointList = [], smoothVersion = [], selectedPoint = 0 } }
                else if upOrDown == Up && ((drawTool state) == newDrawing || (drawTool state) == isMakingNewDrawing) then
                    --trace "Up!" 
                    if (length $ pointList currVec) <= 1 then
                        state   { drawTool = changeToolUp 
                                , currentDrawing = AppVector { pointList = [], smoothVersion = [], selectedPoint = 0 } }
                    else
                        state   {   drawTool = 
                                        --trace ("Draw Tool" ++ (show changeToolUp))
                                        changeToolUp
                                ,   drawings = newVectors
                                ,   currentDrawing = smoothVec }
                                --,   currentDrawing = AppVector { pointList = [] } }, elem)
                else if upOrDown == Down && (drawTool state) == moveDrawing then
                    --trace ("New Selected Drawing: " ++ (show vecSelec))
                    state   { drawTool = changeToolDown
                            , selectedDrawing = vecSelec 
                            , clickedDownPoint = (x, y) }
                else if upOrDown == Up && (drawTool state) == isMovingDrawing then
                    state   { drawTool = changeToolUp }
                else
                    --trace "Other!" 
                    state
            else
                state
    else
        --trace ("Not in plane! (x, y) = (" ++ (show (x, y)))
        state
    where
        changeToolDown =    if drawTool state == newDrawing then
                                isMakingNewDrawing
                            else if drawTool state == moveDrawing then
                                isMovingDrawing
                            else
                                drawTool state
        changeToolUp =      if drawTool state == isMakingNewDrawing then
                                newDrawing
                            else if drawTool state == isMovingDrawing then
                                moveDrawing
                            else
                                drawTool state
                            
        currVec =   currentDrawing state
        smoothedVector = pointList $!  (chaikinOpen 5 0.25 currVec)
        smoothVec = currVec { smoothVersion = smoothedVector }
        newVectors =        --trace ("Adding AppVector with points, " ++ (show (pointList currVec)) ++ "\nSmoothing AppVector with points, " ++ (show (pointList smoothVec)))
                            ((drawings state) ++ [ smoothVec ])
        
        vecSelec = vectorClosestToPoint state (x, y)--selectedDrawing state
drawPaneHandler (EventMotion (x, y)) state elem index =
    if x >= fromIntegral (getX1 (elemCore elem) state) && x <= fromIntegral (getX2 (elemCore elem) state) 
        && y >= fromIntegral (getY1 (elemCore elem) state) && y <= fromIntegral (getY2 (elemCore elem) state) then
            if drawTool state == isMakingNewDrawing then
                if (round y) `mod` 3 == 0 || (round x) `mod` 3 == 0 then  -- Don't capture every point
                    state { currentDrawing =    vectorWithPoint }
                else
                    state
            else if drawTool state == isMovingDrawing then
                state   { drawings =            updatedDrawings
                        , clickedDownPoint =    (x, y) }
            else
                --trace ("(x, y) = (" ++ (show x) ++ ", " ++ (show y) ++ ")")
                state
    else
        state
    where
        currVec = currentDrawing state
        newPointList = (pointList currVec) ++ [ (x, y) ]
        vectorWithPoint =   if Prelude.elem (x, y) (pointList currVec) == True then
                                currVec
                            else
                                (currVec { pointList = newPointList })
                                
        oldPosition =   --trace ("Old Click: " ++ (show $ clickedDownPoint state))
                        (clickedDownPoint state)
        currentSel = if (selectedDrawing state) >= (length (drawings state)) then
                        -- Uhhhhhhh idk
                        AppVector { pointList = [], smoothVersion = [], selectedPoint = 0 }
                    else
                        (drawings state) !! (selectedDrawing state)
        xMotion = x - (fst oldPosition)
        yMotion =   --trace ("Moved Pos X: " ++ (show xMotion))
                    y - (snd oldPosition)
        updatedSelPoints = map (\(px, py) -> (px + xMotion, py + yMotion)) (pointList currentSel)
        updatedSel = currentSel { pointList = updatedSelPoints }
        updatedSelSmooth = updatedSel { smoothVersion = pointList $! (chaikinOpen 5 0.25 updatedSel) }
        updatedDrawings = (fst $ splitAt (selectedDrawing state) (drawings state)) 
                            ++ [ updatedSelSmooth ] ++
                                (snd $ splitAt ((selectedDrawing state) + 1) (drawings state))
drawPaneHandler (EventKey (SpecialKey KeyEsc) Up _ _) state elem index =
    state { drawTool = noState }
drawPaneHandler _ state elem index =
    state