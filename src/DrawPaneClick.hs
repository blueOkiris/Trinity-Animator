module DrawPaneClick where

import Graphics.Gloss.Interface.Pure.Game(Event(..), SpecialKey(..), KeyState(..), Key(..), MouseButton(..))

import State    ( AppWindow(..), AppVector(..), AppState(..), DrawTool
                , chaikinOpen, vectorClosestToPoint, vectorPointClosestToPoint
                , newDrawing, isMakingNewDrawing, moveDrawing, isMovingDrawing, noState, editDrawing, isEditingDrawing )
import GUI(Element(..), DynamicElement(..))
import DrawElement(getX1, getX2, getY1, getY2)
import Lib(replaceElement, pointInRect)

-- Allow for drawing to plane
drawPaneHandler :: Event -> AppState -> (DynamicElement AppState) -> Int -> AppState
drawPaneHandler (EventKey (MouseButton btn) upOrDown modifier (x, y)) state elem index
    | clickedElement && (drawTool state) == newDrawing && upOrDown == Down =
        state       { drawTool =            isMakingNewDrawing 
                    , currentDrawing =      AppVector { pointList = [], smoothVersion = [], selectedPoint = 0 } }
    | clickedElement && (drawTool state) == isMakingNewDrawing && upOrDown == Up =
        if (length $ pointList currVec) <= 1 then
            state   { drawTool =            newDrawing 
                    , currentDrawing =      AppVector { pointList = [], smoothVersion = [], selectedPoint = 0 } }
        else
            state   {   drawTool =          newDrawing
                    ,   drawings =          newVectors
                    ,   currentDrawing =    smoothVec }
    | clickedElement && (drawTool state) == moveDrawing && upOrDown == Down =
        state       { drawTool =            isMovingDrawing
                    , selectedDrawing =     vecSelec 
                    , clickedDownPoint =    (x, y) }
    | clickedElement && (drawTool state) == isMovingDrawing && upOrDown == Up =
        state       { drawTool =            moveDrawing }
    | clickedElement && (drawTool state) == editDrawing && upOrDown == Down =
        state       { drawTool =            isEditingDrawing 
                    , drawings =            editedVectors
                    , clickedDownPoint =    (x, y) }
    | clickedElement && (drawTool state) == isEditingDrawing && upOrDown == Up =
        state       { drawTool =            editDrawing }
    | otherwise = state
    where
        -- General case code
        elementRect =       ((getX1 (elemCore elem) state, getY1 (elemCore elem) state), (getX2 (elemCore elem) state, getY2 (elemCore elem) state))
        clickedElement =    pointInRect (x, y) elementRect && btn == LeftButton
        
        -- New drawing code
        currVec =           currentDrawing state
        smoothedVector =    pointList $!  (chaikinOpen 5 0.25 currVec)
        smoothVec =         currVec { smoothVersion = smoothedVector }
        newVectors =        ((drawings state) ++ [ smoothVec ])
        
        -- Move drawing code
        vecSelec =          vectorClosestToPoint state (x, y)

        -- Edit drawing code
        selectedVec =       (drawings state) !! (selectedDrawing state)
        selPoint =          vectorPointClosestToPoint state (x, y)
        newVec =            selectedVec { selectedPoint = selPoint }
        editedVectors =     replaceElement (selectedDrawing state) (drawings state) newVec
drawPaneHandler (EventMotion (x, y)) state elem index
    | inElement && drawTool state == isMakingNewDrawing && ((round y) `mod` 3 == 0 || (round x) `mod` 3 == 0) =
        state   { currentDrawing =      vectorWithPoint }
    | inElement && drawTool state == isMovingDrawing =
        state   { drawings =            movedDrawings
                , clickedDownPoint =    (x, y) }
    | inElement && drawTool state == isEditingDrawing =
        state   { drawings =            editedDrawings
                , clickedDownPoint =    (x, y) }
    | otherwise = state
    where
        elementRect =       ((getX1 (elemCore elem) state, getY1 (elemCore elem) state), (getX2 (elemCore elem) state, getY2 (elemCore elem) state))
        inElement =         pointInRect (x, y) elementRect

        -- New Drawing code
        currVec =           currentDrawing state
        newPointList =      (pointList currVec) ++ [ (x, y) ]
        vectorWithPoint     | not (Prelude.elem (x, y) (pointList currVec)) = currVec { pointList = newPointList }
                            | otherwise = currVec
                        
        -- Moved Drawing code
        oldPosition =       (clickedDownPoint state)
        currentSel          | (selectedDrawing state) >= (length (drawings state)) = AppVector { pointList = [], smoothVersion = [], selectedPoint = 0 }
                            | otherwise = (drawings state) !! (selectedDrawing state)
        xMotion =           x - (fst oldPosition)
        yMotion =           y - (snd oldPosition)
        movedSelPoints =    map (\(px, py) -> (px + xMotion, py + yMotion)) (pointList currentSel)
        movedSel =          currentSel { pointList = movedSelPoints }
        movedSelSmooth =    movedSel { smoothVersion = pointList $! (chaikinOpen 5 0.25 movedSel) }
        movedDrawings =     replaceElement (selectedDrawing state) (drawings state) movedSelSmooth

        -- Edit drawing code
        -- oldPosition from move above
        -- currentSel from move above
        -- xMotion & yMotion from above
        -- Now here's the distance
        currentSelPoint =   selectedPoint currentSel
        points =            pointList currentSel
        currPoint =         (pointList currentSel) !! currentSelPoint
        editedSelPoints =   replaceElement currentSelPoint points ((fst currPoint) + xMotion, (snd currPoint) + yMotion)
        editedSel =         currentSel { pointList = editedSelPoints }
        editedSelSmooth =   editedSel { smoothVersion = pointList $! (chaikinOpen 5 0.25 movedSel) }
        editedDrawings =    replaceElement (selectedDrawing state) (drawings state) editedSelSmooth
drawPaneHandler (EventKey (SpecialKey KeyEsc) Up _ _) state elem index =
    state { drawTool = noState }
drawPaneHandler _ state elem index =
    state