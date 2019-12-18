module GUIObjects where

import Graphics.Gloss(red, white)
import Graphics.Gloss.Interface.Pure.Game(Event(..), SpecialKey(..), KeyState(..), Key(..), MouseButton(..))
import Debug.Trace
import Control.DeepSeq

import State(AppWindow(..), AppVector(..), chaikinOpen, AppState(..), DrawTool, newDrawing, isMakingNewDrawing, moveDrawing)
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
                if upOrDown == Down && (drawTool state) == newDrawing then
                    --trace "Down!"
                    state   { drawTool = changeToolDown 
                            , currentDrawing = AppVector { pointList = [], smoothVersion = [] } }
                else if upOrDown == Up && ((drawTool state) == newDrawing || (drawTool state) == isMakingNewDrawing) then
                    --trace "Up!" 
                    state   {   drawTool = 
                                    --trace ("Draw Tool" ++ (show changeToolUp))
                                    changeToolUp
                            ,   drawings = newVectors
                            ,   currentDrawing = smoothVec }
                            --,   currentDrawing = AppVector { pointList = [] } }, elem)
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
                            else
                                drawTool state
        changeToolUp =      if drawTool state == isMakingNewDrawing then
                                newDrawing
                            else
                                drawTool state
                            
        currVec = currentDrawing state
        smoothedVector = pointList $!  (chaikinOpen 7 0.25 currVec)
        smoothVec = currVec { smoothVersion = smoothedVector }
        newVectors =        --trace ("Adding AppVector with points, " ++ (show (pointList currVec)) ++ "\nSmoothing AppVector with points, " ++ (show (pointList smoothVec)))
                            ((drawings state) ++ [ smoothVec ])
drawPaneHandler (EventMotion (x, y)) state elem index =
    if x >= fromIntegral (getX1 (elemCore elem) state) && x <= fromIntegral (getX2 (elemCore elem) state) 
        && y >= fromIntegral (getY1 (elemCore elem) state) && y <= fromIntegral (getY2 (elemCore elem) state) then
            if drawTool state == isMakingNewDrawing then
                if (round y) `mod` 3 == 0 || (round x) `mod` 3 == 0 then  -- Don't capture every point
                    state { currentDrawing = vectorWithPoint }
                else
                    state
            else
                --trace ("(x, y) = (" ++ (show x) ++ ", " ++ (show y) ++ ")")
                state
    else
        state
    where
        currVec = currentDrawing state
        newPointList = (pointList currVec) ++ [ (x, y) ]
        vectorWithPoint = (currVec { pointList = newPointList })
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
                , currentDrawing = newCurrent }
    else
        state
    where
        drawingsWOutCurr = fst (splitAt ((length (drawings state)) - 1) (drawings state))
        newCurrent = 
                    if length drawingsWOutCurr == 0 then
                        AppVector { pointList = [], smoothVersion = [] }
                    else
                        drawingsWOutCurr !! ((length drawingsWOutCurr) - 1)
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
        bgImg = if (drawTool state) == moveDrawing then
                    moveIconSelected state
                else
                    moveIcon state
        newElemCore = (elemCore elem) { backImage = bgImg }
        newDynElem = elem { elemCore = newElemCore }
        
        newElements = (fst (splitAt index (elements state))) ++ [newDynElem] ++ (snd (splitAt (index + 1) (elements state)))
        newState = state { elements = newElements }

    