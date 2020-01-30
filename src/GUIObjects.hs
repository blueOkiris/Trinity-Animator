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
import Lib(pointInRect, replaceElement, deleteElement)

-- A default option for handlers that does nothing
defaultElementEventHandler :: Event -> AppState -> (DynamicElement AppState) -> Int -> AppState
defaultElementEventHandler event state elem index =
    state

-- Button handling
drawIconHandler :: Event -> AppState -> (DynamicElement AppState) -> Int -> AppState
drawIconHandler (EventKey (MouseButton btn) upOrDown modifier (x, y)) state elem index
    | clickedElement && upOrDown == Down = state { drawTool = newDrawing }
    | otherwise = state
    where
        elementRect =       ((getX1 (elemCore elem) state, getY1 (elemCore elem) state), (getX2 (elemCore elem) state, getY2 (elemCore elem) state))
        clickedElement =    pointInRect (x, y) elementRect && btn == LeftButton
drawIconHandler (EventKey (Char 'p') Up _ _) state elem index
    | not $ Prelude.elem (drawTool state) 
        [ isMakingNewDrawing
        , isMovingDrawing
        , isEditingDrawing ] =
            state { drawTool = newDrawing }
    | otherwise = state
drawIconHandler _ state elem index =
    state

editIconHandler :: Event -> AppState -> (DynamicElement AppState) -> Int -> AppState
editIconHandler (EventKey (MouseButton btn) upOrDown modifier (x, y)) state elem index
    | clickedElement && upOrDown == Down && (drawTool state) /= editDrawing = state { drawTool = editDrawing }
    | otherwise = state
    where
        elementRect =       ((getX1 (elemCore elem) state, getY1 (elemCore elem) state), (getX2 (elemCore elem) state, getY2 (elemCore elem) state))
        clickedElement =    pointInRect (x, y) elementRect && btn == LeftButton
editIconHandler (EventKey (Char 'e') Up _ _) state elem index
    | not $ Prelude.elem (drawTool state)
        [ isMakingNewDrawing
        , isMovingDrawing
        , isEditingDrawing ] =
            state { drawTool = editDrawing }
    | otherwise = state
editIconHandler (EventKey (SpecialKey KeyLeft) Up _ _) state elem index
    | (drawTool state) == editDrawing = state { drawings = editedDrawings }
    | otherwise = state
    where
        currDrawing =       (drawings state) !! (selectedDrawing state)
        newSelPoint         | (selectedPoint currDrawing ) > 1 = (selectedPoint currDrawing) - 1
                            | otherwise = (length (pointList currDrawing)) - 1
        editedDrawing =     currDrawing { selectedPoint = newSelPoint }
        editedDrawings =    replaceElement (selectedDrawing state) (drawings state) editedDrawing
editIconHandler (EventKey (SpecialKey KeyRight) Up _ _) state elem index
    | (drawTool state) == editDrawing = state { drawings = editedDrawings }
    | otherwise = state
    where
        currDrawing =       (drawings state) !! (selectedDrawing state)
        newSelPoint         | (selectedPoint currDrawing ) < (length $ pointList currDrawing) - 1 = (selectedPoint currDrawing) + 1
                            | otherwise = 0
        editedDrawing =     currDrawing { selectedPoint = newSelPoint }
        editedDrawings =    replaceElement (selectedDrawing state) (drawings state) editedDrawing
editIconHandler (EventKey (SpecialKey KeyDelete) Up _ _) state elem index
    | (drawTool state) == editDrawing = state { drawings = drawingsWEditedCurr }
    | otherwise = state
    where
        currDrawing =           (drawings state) !! (selectedDrawing state)
        points =                pointList currDrawing
        selPointInd =           selectedPoint currDrawing
        editedPoints =          deleteElement selPointInd points
        editedSel =             currDrawing { pointList = editedPoints }
        editedSelectedDrawing = editedSel { smoothVersion = pointList $! (chaikinOpen 5 0.25 editedSel) }
        drawingsWEditedCurr =   replaceElement (selectedDrawing state) (drawings state) editedSelectedDrawing
editIconHandler _ state elem index =
    state

moveIconHandler :: Event -> AppState -> (DynamicElement AppState) -> Int -> AppState
moveIconHandler (EventKey (MouseButton btn) upOrDown modifier (x, y)) state elem index
    | clickedElement && upOrDown == Down && (drawTool state) /= moveDrawing = state { drawTool = moveDrawing }
    | otherwise = state
    where
        elementRect =       ((getX1 (elemCore elem) state, getY1 (elemCore elem) state), (getX2 (elemCore elem) state, getY2 (elemCore elem) state))
        clickedElement =    pointInRect (x, y) elementRect && btn == LeftButton
moveIconHandler (EventKey (Char 'm') Up _ _) state elem index
    | not $ Prelude.elem (drawTool state)
        [ isMakingNewDrawing
        , isMovingDrawing
        , isEditingDrawing ] =
            state { drawTool = moveDrawing }
    | otherwise = state
moveIconHandler (EventKey (SpecialKey KeyDelete) Up _ _) state elem index
    | (drawTool state) == moveDrawing =
        state   { drawings =        drawingsWOutCurr
                , currentDrawing =  newCurrent 
                , selectedDrawing = newSelection }
    | otherwise = state
    where
        drawingsWOutCurr =  deleteElement (selectedDrawing state) (drawings state)
        newCurrent          | length drawingsWOutCurr == 0 = AppVector { pointList = [], smoothVersion = [], selectedPoint = 0 }
                            | otherwise = drawingsWOutCurr !! ((length drawingsWOutCurr) - 1)
        newSelection        | length drawingsWOutCurr == 0 = 0
                            | otherwise = (length drawingsWOutCurr) - 1
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
    newState
    where
        bgImg  | (drawTool state) == newDrawing || (drawTool state) == isMakingNewDrawing = drawIconSelected state
                | otherwise = drawIcon state
        newElemCore =   (elemCore elem) { backImage = bgImg }
        newDynElem =    elem { elemCore = newElemCore }
        newElements =   replaceElement index (elements state) newDynElem
        newState =      state { elements = newElements }

updateMoveIconFunc :: Float -> AppState -> (DynamicElement AppState) -> Int -> AppState
updateMoveIconFunc seconds state elem index =
    newState
    where
        bgImg   | (drawTool state) == moveDrawing || (drawTool state) == isMovingDrawing = moveIconSelected state
                | otherwise = moveIcon state
        newElemCore =   (elemCore elem) { backImage = bgImg }
        newDynElem =    elem { elemCore = newElemCore }
        newElements =   replaceElement index (elements state) newDynElem
        newState =      state { elements = newElements }

updateEditIconFunc :: Float -> AppState -> (DynamicElement AppState) -> Int -> AppState
updateEditIconFunc seconds state elem index =
    newState
    where
        bgImg   | (drawTool state) == editDrawing || (drawTool state) == isEditingDrawing = editIconSelected state
                | otherwise = editIcon state
        newElemCore =   (elemCore elem) { backImage = bgImg }
        newDynElem =    elem { elemCore = newElemCore }
        newElements =   replaceElement index (elements state) newDynElem
        newState =      state { elements = newElements }

    