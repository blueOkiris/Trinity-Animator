module Main where

import Graphics.Gloss   ( Display(..)
                        , Picture(..), pictures, translate, circleSolid, rectangleSolid, line
                        , color, white, black, red, blue, green, yellow, magenta )
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game(play)
import Debug.Trace

import State(AppState(..), AppVector(..), chaikinOpen, AppWindow(..), DrawTool, newDrawing, isMakingNewDrawing, moveDrawing)
import GUI(Element(..), DynamicElement(..), Alignment, alignCenter, alignLeft, alignRight, alignTop, alignBottom, alignStretch)
import GUIObjects(updateElement)
import Init(startState)
import DrawElement
import Event(handler)

-- Draw all the vectors on the screen
-- Color not implemented, so black for now
drawVector :: Int -> Bool -> AppVector -> Picture
drawVector index smooth vector =
    if length (pointList smoothVec) == 0 || index == (length (pointList smoothVec)) - 1 then
        Blank
    else if length (pointList smoothVec) == 1 then
        translate x y (color black $ circleSolid 1)
    else
        pictures [ vectorLine ]--, drawVector (index + 1) Prelude.False smoothVec ]
    where
        smoothVec = if smooth == True then  -- only smooth the first time
                        AppVector { pointList = smoothVersion vector, smoothVersion = smoothVersion vector}
                    else
                        vector
        (x, y) = (pointList smoothVec) !! index
        --myTwoPoints = take 2 (snd (splitAt index (pointList smoothVec)))
        flippedPoints = map (\(x, y) -> (x, -y)) (pointList smoothVec)--myTwoPoints
        vectorLine = color black $ line
                        --trace ("Drawing line with points: " ++ (show flippedPoints)) 
                        flippedPoints

-- Draw GUI elements in a list of elements
drawElements :: AppState -> Picture
drawElements state =
    --trace ("Drawing Elements:\n")
    pictures (map (drawElement state) (map (elemCore) (elements state)))

-- Draw everything in the window
render :: AppState -> Picture
render state =
    applyViewPortToPicture
        (viewPortInit   { viewPortTranslate = (-winWidth / 2, winHeight / 2) })
        (pictures   [ Blank
                    --, translate 0 0 (color red $ circleSolid 100) ])
                    , drawElements state 
                    , pictures vectorPictures
                    --, line [ (0, 0), (1280, -720) ]
                    --, --trace ("Rendering current vector with points: " ++ (show (pointList (currentDrawing state))) ++ "\nTool State: " ++ (show (drawTool state)))
                    --    (drawVector 0 (currentDrawing state)) ])
                    , currentVecPic ])
    where
        winWidth = fromIntegral (width $ window state)
        winHeight = fromIntegral (height $ window state)
        vectorPictures = map (drawVector 0 True) (drawings state)

        -- Draw little dots at each point
        dots = pictures (map (\(x, y) -> translate x (-y) (color red (circleSolid 4))) (pointList (currentDrawing state)))
        currentVecPic = if drawTool state == newDrawing then
                            (drawVector 0 True (currentDrawing state))
                        else if drawTool state == isMakingNewDrawing then
                            (drawVector 0 Prelude.False (currentDrawing state))
                        else if drawTool state == moveDrawing then
                            pictures [ drawVector 0 True (currentDrawing state), dots ]
                        else
                            Blank

updateAll :: Int -> Float -> AppState -> AppState
updateAll index seconds state =
    if index == length (elements state) then
        state
    else
        updateAll (index + 1) seconds newState
    where
        -- First, update the state and get a new updated element
        (newState, updatedElement) = updateElement seconds state ((elements state) !! index)

        -- Then copy the updated element into the new state at the proper index
        newElements = (fst (splitAt index (elements state))) ++ [updatedElement] ++ (snd (splitAt (index + 1) (elements state)))
        newNewState = newState { elements = newElements }

-- Update everything based on new state
update :: Float -> AppState -> AppState
update seconds state =
    updateAll 0 seconds state        

-- This initalizes the "play" command
-- play is of type :: Display -> Color -> Int -> T -> (T -> Picture) -> (Event -> T -> T) -> (Float -> T -> T)
-- In our case the 'T' is our global state
-- Each of these corresponds to (in order):
--  * physically drawn window data,
--  * window background color
--  * fps
--  * render function
--  * event handler
--  * update function
main :: IO ()
main = 
    play disp bg numFrames startState render handler update
    where
        disp =      display $ window startState
        bg =        bgColor $ window startState
        numFrames = fps     $ window startState
