module GUIObjects where

import Graphics.Gloss(red, white)
import Graphics.Gloss.Interface.Pure.Game(Event(..), SpecialKey(..), KeyState(..), Key(..), MouseButton(..))
import Debug.Trace
import Control.DeepSeq

import State    ( AppWindow(..), AppVector(..), AppState(..), DrawTool
                , chaikinOpen, vectorClosestToPoint
                , newDrawing, isMakingNewDrawing, moveDrawing, isMovingDrawing, noState )
import GUI(Element(..), DynamicElement(..))
import DrawElement(getX1, getX2, getY1, getY2)

-- A default option for handlers that does nothing
defaultElementEventHandler :: Event -> AppState -> (DynamicElement AppState) -> Int -> AppState
defaultElementEventHandler event state elem index =
    state

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
drawPaneHandler (EventKey (Char 'm') Up _ _) state elem index =
    if drawTool state /= isMakingNewDrawing then
        state { drawTool = moveDrawing }
    else
        state
drawPaneHandler (EventKey (Char 'p') Up _ _) state elem index =
    if drawTool state /= isMakingNewDrawing then
        state { drawTool = newDrawing }
    else
        state
drawPaneHandler (EventKey (SpecialKey KeyDelete) Up _ _) state elem index =
    if drawTool state == moveDrawing then
        state   { drawings = drawingsWOutCurr
                , currentDrawing = newCurrent 
                , selectedDrawing = newSelection }
    else
        state
    where
        drawingsWOutCurr =  (fst (splitAt (selectedDrawing state) (drawings state)))
                                ++ (snd (splitAt ((selectedDrawing state) + 1) (drawings state)))
        newCurrent = 
                    if length drawingsWOutCurr == 0 then
                        AppVector { pointList = [], smoothVersion = [], selectedPoint = 0 }
                    else
                        drawingsWOutCurr !! ((length drawingsWOutCurr) - 1)
        newSelection =  if length drawingsWOutCurr == 0 then
                            0
                        else
                            (length drawingsWOutCurr) - 1
drawPaneHandler _ state elem index =
    state

-- Button handling
drawIconHandler :: Event -> AppState -> (DynamicElement AppState) -> Int -> AppState
drawIconHandler (EventKey (MouseButton btn) upOrDown modifier (x, y)) state elem index =
    if x >= fromIntegral (getX1 (elemCore elem) state) && x <= fromIntegral (getX2 (elemCore elem) state) 
        && y >= fromIntegral (getY1 (elemCore elem) state) && y <= fromIntegral (getY2 (elemCore elem) state) then
            if upOrDown == Down then
                if (drawTool state) == newDrawing || (drawTool state) == isMakingNewDrawing then
                    state
                else
                    state { drawTool = newDrawing }
            else
                state
    else
        state
drawIconHandler _ state elem index =
    state

moveIconHandler :: Event -> AppState -> (DynamicElement AppState) -> Int -> AppState
moveIconHandler (EventKey (MouseButton btn) upOrDown modifier (x, y)) state elem index =
    if x >= fromIntegral (getX1 (elemCore elem) state) && x <= fromIntegral (getX2 (elemCore elem) state) 
        && y >= fromIntegral (getY1 (elemCore elem) state) && y <= fromIntegral (getY2 (elemCore elem) state) then
            if upOrDown == Down then
                if (drawTool state) == moveDrawing then
                    state
                else
                    state { drawTool = moveDrawing }
            else
                state
    else
        state
moveIconHandler _ state elem index =
    state

-- A default option for update that does nothing
defaultElementUpdate :: Float -> AppState ->  (DynamicElement AppState) -> Int -> AppState
defaultElementUpdate seconds state elem index =
    state

-- Actually add a new drawing 
drawPaneUpdate :: Float -> AppState -> (DynamicElement AppState) -> Int -> AppState
drawPaneUpdate seconds state elem index =
    -- Smooth the drawing line
    state

-- Change icons for button state
updateDrawIconFunc :: Float -> AppState -> (DynamicElement AppState) -> Int -> AppState
updateDrawIconFunc seconds state elem index =
    --trace ("Draw Tool State: " ++ (show (drawTool state)))
    newState
    where
        bgImg = if (drawTool state) == newDrawing || (drawTool state) == isMakingNewDrawing then
                    drawIconSelected state
                else
                    drawIcon state
        newElemCore = (elemCore elem) { backImage = bgImg }
        newDynElem = elem { elemCore = newElemCore }
        
        newElements = (fst (splitAt index (elements state))) ++ [newDynElem] ++ (snd (splitAt (index + 1) (elements state)))
        newState = state { elements = newElements }

updateMoveIconFunc :: Float -> AppState -> (DynamicElement AppState) -> Int -> AppState
updateMoveIconFunc seconds state elem index =
    newState
    where
        bgImg = if (drawTool state) == moveDrawing || (drawTool state) == isMovingDrawing then
                    moveIconSelected state
                else
                    moveIcon state
        newElemCore = (elemCore elem) { backImage = bgImg }
        newDynElem = elem { elemCore = newElemCore }
        
        newElements = (fst (splitAt index (elements state))) ++ [newDynElem] ++ (snd (splitAt (index + 1) (elements state)))
        newState = state { elements = newElements }

    