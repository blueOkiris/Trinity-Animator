module GUIObjects where

import Graphics.Gloss(red, white)
import Graphics.Gloss.Interface.Pure.Game(Event(..), SpecialKey(..), KeyState(..), Key(..), MouseButton(..))
import Debug.Trace

import State(AppWindow(..), Vector(..), AppState(..), DrawTool, newDrawing, isMakingNewDrawing, moveDrawing)
import GUI(Element(..), DynamicElement(..))
import DrawElement(getX1, getX2, getY1, getY2)

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
    if x >= fromIntegral (getX1 elem state) && x <= fromIntegral (getX2 elem state) 
        && y >= fromIntegral (getY1 elem state) && y <=  fromIntegral (getY2 elem state) then
            if btn == LeftButton then
                if upOrDown == Down then
                    --trace "Down!"
                    (state { drawTool = changeToolDown }, elem)
                else if upOrDown == Up then
                    --trace "Up!" 
                    (state  {   drawTool = 
                                    --trace ("Draw Tool" ++ (show changeToolUp))
                                    changeToolUp
                            ,   drawings = newVectors 
                            ,   currentDrawing = Vector { pointList = [] } }, elem)
                else
                    --trace "Other!" 
                    (state, elem)
            else
                (state, elem)
    else
        --trace ("Not in plane! (x, y) = (" ++ (show (x, y)))
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
        newVectors =        --trace ("Adding vector with points, " ++ (show (pointList (currentDrawing state))))
                            ((drawings state) ++ [currentDrawing state])
drawPaneHandler (EventMotion (x, y)) (state, elem) =
    if x >= fromIntegral (getX1 elem state) && x <= fromIntegral (getX2 elem state) 
        && y >= fromIntegral (getY1 elem state) && y <= fromIntegral (getY2 elem state) then
            if drawTool state == isMakingNewDrawing then
                (state { currentDrawing = vectorWithPoint }, elem)
            else
                (state, elem)
    else
        (state, elem)
    where
        currVec = currentDrawing state
        newPointList = (pointList currVec) ++ [ (x, y) ]
        vectorWithPoint = currVec { pointList = newPointList }
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
applyHandler :: Event -> AppState -> (DynamicElement AppState) -> (AppState, (DynamicElement AppState))
applyHandler event state elem =
    (newState, elem { elemCore = handledElemCore })
    where
        (newState, handledElemCore) = (keyEventElem elem) event (state, (elemCore elem))