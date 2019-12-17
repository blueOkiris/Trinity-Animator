module GUIObjects where

import Graphics.Gloss(red, white)
import Graphics.Gloss.Interface.Pure.Game(Event(..), SpecialKey(..), KeyState(..), Key(..), MouseButton(..))
import Debug.Trace

import State(AppWindow(..), Vector(..), chaikinOpen, AppState(..), DrawTool, newDrawing, isMakingNewDrawing, moveDrawing)
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
                    (state  { drawTool = changeToolDown 
                            , currentDrawing = Vector { pointList = [] } }, elem)
                else if upOrDown == Up then
                    --trace "Up!" 
                    (state  {   drawTool = 
                                    --trace ("Draw Tool" ++ (show changeToolUp))
                                    changeToolUp
                            ,   drawings = newVectors
                            ,   currentDrawing = smoothVec }, elem)
                            --,   currentDrawing = Vector { pointList = [] } }, elem)
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
        currVec = currentDrawing state
        smoothVec = --chaikinOpen 1 0.25 currVec
                    currVec
        newVectors =        --trace ("Adding vector with points, " ++ (show (pointList currVec)) ++ "\nSmoothing vector with points, " ++ (show (pointList smoothVec)))
                            ((drawings state) ++ [ smoothVec ])
drawPaneHandler (EventMotion (x, y)) (state, elem) =
    if x >= fromIntegral (getX1 elem state) && x <= fromIntegral (getX2 elem state) 
        && y >= fromIntegral (getY1 elem state) && y <= fromIntegral (getY2 elem state) then
            if drawTool state == isMakingNewDrawing then
                if (round y) `mod` 1 == 0 || (round x) `mod` 1 == 0 then  -- Don't capture every point
                    (state { currentDrawing = vectorWithPoint }, elem)
                else
                    (state, elem)
            else
                --trace ("(x, y) = (" ++ (show x) ++ ", " ++ (show y) ++ ")")
                (state, elem)
    else
        (state, elem)
    where
        currVec = currentDrawing state
        newPointList = (pointList currVec) ++ [ (x, y) ]
        vectorWithPoint = (currVec { pointList = newPointList })
drawPaneHandler (EventKey (Char 'm') Up _ _) (state, elem) =
    if drawTool state /= isMakingNewDrawing then
        (state { drawTool = moveDrawing }, elem)
    else
        (state, elem)
drawPaneHandler (EventKey (Char 'p') Up _ _) (state, elem) =
    if drawTool state /= isMakingNewDrawing then
        (state { drawTool = newDrawing }, elem)
    else
        (state, elem)
drawPaneHandler (EventKey (SpecialKey KeyDelete) Up _ _) (state, elem) =
    if drawTool state == moveDrawing then
        (state  { drawings = drawingsWOutCurr
                , currentDrawing = newCurrent }, elem)
    else
        (state, elem)
    where
        drawingsWOutCurr = fst (splitAt ((length (drawings state)) - 1) (drawings state))
        newCurrent = 
                    if length drawingsWOutCurr == 0 then
                        Vector { pointList = [] }
                    else
                        drawingsWOutCurr !! ((length drawingsWOutCurr) - 1)
drawPaneHandler _ (state, elem) =
    (state, elem)

-- Actually add a new drawing 
drawPaneUpdate :: Float -> (AppState, Element) -> (AppState, Element)
drawPaneUpdate seconds (state, elem) =
    -- Smooth the drawing line
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