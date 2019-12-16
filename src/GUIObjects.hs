module GUIObjects where

import Graphics.Gloss(red, white)
import Graphics.Gloss.Interface.Pure.Game(Event(..), SpecialKey(..), KeyState(..), Key(..), MouseButton(..))

import State(AppWindow(..), AppState(..), DrawTool, newDrawing, isMakingNewDrawing, moveDrawing)
import GUI(Element(..), DynamicElement(..))

-- A default option for update that does nothing
defaultElementUpdate :: Float -> (AppState, Element) -> (AppState, Element)
defaultElementUpdate seconds (state, elem) =
    (state, elem)

-- A default option for handlers that does nothing
defaultElementEventHandler :: Event -> (AppState,Element) -> (AppState, Element)
defaultElementEventHandler event (state, elem) =
    (state, elem)

-- Allow for drawing to plane
drawPaneHandler :: Event -> (AppState, Element) -> (AppState, Element)
drawPaneHandler (EventKey (MouseButton btn) upOrDown modifier (x, y)) (state, elem) =
    -- First check to make sure we're actually in the element
    if btn == LeftButton then
        if upOrDown == Down then
            (state { drawTool = changeToolDown }, elem)
        else if upOrDown == Up then
            (state { drawTool = changeToolUp }, elem)
        else
            (state, elem)
    else
        (state, elem)
    where
        changeToolDown =    if drawTool state == newDrawing then
                                isMakingNewDrawing
                            else
                                drawTool state
        changeToolUp =      if drawTool state == isMakingNewDrawing then
                                newDrawing
                            else
                                drawTool state
drawPaneHandler _ (state, elem) =
    (state, elem)

-- Actually add a new drawing 
drawPaneUpdate :: Float -> (AppState, Element) -> (AppState, Element)
drawPaneUpdate seconds (state, elem) =
    (state, elem)

-- Apply the update and event functions
-- Update an element inside a dynamic element based on the d.e.'s update function
updateElement :: Float -> AppState -> (DynamicElement AppState) -> (AppState, (DynamicElement AppState))
updateElement seconds state de =
    (updatedState, de  { elemCore =    updatedCore })
    where
        updateFunc =    updateElem de
        elem =          elemCore de
        (updatedState, updatedCore) =   updateFunc seconds (state, elem)

-- Handle events for and element
applyHandler :: Event -> AppState -> (DynamicElement AppState) -> (DynamicElement AppState)
applyHandler event state elem =
    elem { elemCore = handledElemCore }
    where
        (newState, handledElemCore) = (keyEventElem elem) event (state, (elemCore elem))