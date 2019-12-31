module GUIObjects where

import Graphics.Gloss(red, white)
import Graphics.Gloss.Interface.Pure.Game(Event(..), SpecialKey(..), KeyState(..), Key(..), MouseButton(..))
import Debug.Trace
import Control.DeepSeq

import State    ( AppWindow(..), AppVector(..), AppState(..), DrawTool
                , chaikinOpen
                , newDrawing, isMakingNewDrawing, moveDrawing, isMovingDrawing, noState, editDrawing, isEditingDrawing )
import GUI(Element(..), DynamicElement(..))
import DrawElement(getX1, getX2, getY1, getY2)

-- A default option for handlers that does nothing
defaultElementEventHandler :: Event -> AppState -> (DynamicElement AppState) -> Int -> AppState
defaultElementEventHandler event state elem index =
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
drawIconHandler (EventKey (Char 'p') Up _ _) state elem index =
    if not $ Prelude.elem (drawTool state) [ isMakingNewDrawing, isMovingDrawing, isEditingDrawing ] then
        state { drawTool = newDrawing }
    else
        state
drawIconHandler _ state elem index =
    state

editIconHandler :: Event -> AppState -> (DynamicElement AppState) -> Int -> AppState
editIconHandler (EventKey (MouseButton btn) upOrDown modifier (x, y)) state elem index =
    if x >= fromIntegral (getX1 (elemCore elem) state) && x <= fromIntegral (getX2 (elemCore elem) state) 
        && y >= fromIntegral (getY1 (elemCore elem) state) && y <= fromIntegral (getY2 (elemCore elem) state) then
            if upOrDown == Down then
                if (drawTool state) == editDrawing then
                    state
                else
                    state { drawTool = editDrawing }
            else
                state
    else
        state
editIconHandler (EventKey (Char 'e') Up _ _) state elem index =
    if not $ Prelude.elem (drawTool state) [ isMakingNewDrawing, isMovingDrawing, isEditingDrawing ] then
        state { drawTool = editDrawing }
    else
        state
editIconHandler _ state elem index =
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
moveIconHandler (EventKey (Char 'm') Up _ _) state elem index =
    if not $ Prelude.elem (drawTool state) [ isMakingNewDrawing, isMovingDrawing, isEditingDrawing ] then
        state { drawTool = moveDrawing }
    else
        state
moveIconHandler (EventKey (SpecialKey KeyDelete) Up _ _) state elem index =
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

updateEditIconFunc :: Float -> AppState -> (DynamicElement AppState) -> Int -> AppState
updateEditIconFunc seconds state elem index =
    newState
    where
        bgImg = if (drawTool state) == editDrawing || (drawTool state) == isEditingDrawing then
                    editIconSelected state
                else
                    editIcon state
        newElemCore = (elemCore elem) { backImage = bgImg }
        newDynElem = elem { elemCore = newElemCore }
        
        newElements = (fst (splitAt index (elements state))) ++ [newDynElem] ++ (snd (splitAt (index + 1) (elements state)))
        newState = state { elements = newElements }

    