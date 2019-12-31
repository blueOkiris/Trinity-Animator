module DrawPaneClick where

import Graphics.Gloss.Interface.Pure.Game(Event(..), SpecialKey(..), KeyState(..), Key(..), MouseButton(..))

import State    ( AppWindow(..), AppVector(..), AppState(..), DrawTool
                , chaikinOpen, vectorClosestToPoint, vectorPointClosestToPoint
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
                else if upOrDown == Down && (drawTool state) == editDrawing then
                    --trace ("New Selected Drawing: " ++ (show vecSelec))
                    state   { drawTool = changeToolDown 
                            , drawings = editedVectors
                            , clickedDownPoint = (x, y) }
                else if upOrDown == Up && (drawTool state) == isEditingDrawing then
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
        -- General case code
        changeToolDown =    if drawTool state == newDrawing then
                                isMakingNewDrawing
                            else if drawTool state == moveDrawing then
                                isMovingDrawing
                            else if drawTool state == editDrawing then
                                isEditingDrawing
                            else
                                drawTool state
        changeToolUp =      if drawTool state == isMakingNewDrawing then
                                newDrawing
                            else if drawTool state == isMovingDrawing then
                                moveDrawing
                            else if drawTool state == isEditingDrawing then
                                editDrawing
                            else
                                drawTool state
        
        -- New drawing code
        currVec =   currentDrawing state
        smoothedVector = pointList $!  (chaikinOpen 5 0.25 currVec)
        smoothVec = currVec { smoothVersion = smoothedVector }
        newVectors =        --trace ("Adding AppVector with points, " ++ (show (pointList currVec)) ++ "\nSmoothing AppVector with points, " ++ (show (pointList smoothVec)))
                            ((drawings state) ++ [ smoothVec ])
        
        -- Move drawing code
        vecSelec = vectorClosestToPoint state (x, y)--selectedDrawing state

        -- Edit drawing code
        selectedVec = (drawings state) !! (selectedDrawing state)
        selPoint = vectorPointClosestToPoint state (x, y)
        newVec = selectedVec { selectedPoint = selPoint }
        editedVectors = (fst $ splitAt (selectedDrawing state) (drawings state))
                            ++ [ newVec ] ++ (snd $ splitAt ((selectedDrawing state) + 1) (drawings state))
drawPaneHandler (EventMotion (x, y)) state elem index =
    if x >= fromIntegral (getX1 (elemCore elem) state) && x <= fromIntegral (getX2 (elemCore elem) state) 
        && y >= fromIntegral (getY1 (elemCore elem) state) && y <= fromIntegral (getY2 (elemCore elem) state) then
            if drawTool state == isMakingNewDrawing then
                if (round y) `mod` 3 == 0 || (round x) `mod` 3 == 0 then  -- Don't capture every point
                    state { currentDrawing =    vectorWithPoint }
                else
                    state
            else if drawTool state == isMovingDrawing then
                state   { drawings =            movedDrawings
                        , clickedDownPoint =    (x, y) }
            else if drawTool state == isEditingDrawing then
                state   { drawings =            editedDrawings
                        , clickedDownPoint =    (x, y) }
            else
                --trace ("(x, y) = (" ++ (show x) ++ ", " ++ (show y) ++ ")")
                state
    else
        state
    where
        -- New Drawing code
        currVec = currentDrawing state
        newPointList = (pointList currVec) ++ [ (x, y) ]
        vectorWithPoint =   if Prelude.elem (x, y) (pointList currVec) == True then
                                currVec
                            else
                                (currVec { pointList = newPointList })
                        
        -- Moved Drawing code
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
        movedSelPoints = map (\(px, py) -> (px + xMotion, py + yMotion)) (pointList currentSel)
        movedSel = currentSel { pointList = movedSelPoints }
        movedSelSmooth = movedSel { smoothVersion = pointList $! (chaikinOpen 5 0.25 movedSel) }
        movedDrawings = (fst $ splitAt (selectedDrawing state) (drawings state)) 
                            ++ [ movedSelSmooth ] ++
                                (snd $ splitAt ((selectedDrawing state) + 1) (drawings state))

        -- Edit drawing code
        -- oldPosition from move above
        -- currentSel from move above
        -- xMotion & yMotion from above
        -- Now here's the distance
        currentSelPoint = selectedPoint currentSel
        points = pointList currentSel
        currPoint = (pointList currentSel) !! currentSelPoint
        editedSelPoints =   (fst $ splitAt currentSelPoint points)
                                ++ [ ((fst currPoint) + xMotion, (snd currPoint) + yMotion) ]
                                    ++ (snd $ splitAt (currentSelPoint + 1) points)
        editedSel = currentSel { pointList = editedSelPoints }
        editedSelSmooth = editedSel { smoothVersion = pointList $! (chaikinOpen 5 0.25 movedSel) }
        editedDrawings =    (fst $ splitAt (selectedDrawing state) (drawings state)) 
                                ++ [ editedSelSmooth ] ++
                                    (snd $ splitAt ((selectedDrawing state) + 1) (drawings state))
drawPaneHandler (EventKey (SpecialKey KeyEsc) Up _ _) state elem index =
    state { drawTool = noState }
drawPaneHandler _ state elem index =
    state