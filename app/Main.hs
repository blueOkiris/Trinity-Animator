module Main where

import Graphics.Gloss   ( Display(..)
                        , Picture(..), pictures, translate, circleSolid, rectangleSolid, line
                        , color, white, black, red, blue, green, yellow, magenta )
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game(playIO)
import Debug.Trace

import State    ( AppState(..), AppVector(..), AppWindow(..), DrawTool, chaikinOpen
                , newDrawing, isMakingNewDrawing, moveDrawing, isMovingDrawing, editDrawing, isEditingDrawing )
import GUI(Element(..), DynamicElement(..), Alignment, alignCenter, alignLeft, alignRight, alignTop, alignBottom, alignStretch, pngToPicture)
import Init(startState)
import DrawElement
import Event(handler)
import System.IO(stdout, hSetBuffering, BufferMode(..))
import Lib((?), Cond(..))

-- Draw all the vectors on the screen
-- Color not implemented, so black for now
drawVector :: Int -> Bool -> AppVector -> Picture
drawVector index smooth vector
    | length (pointList smoothVec) == 0 || index == (length (pointList smoothVec)) - 1 = Blank
    | length (pointList smoothVec) == 1 = translate x y (color black $ circleSolid 1)
    | otherwise = pictures [ vectorLine ]
    where
         -- only smooth the first time
        smoothVec = smooth == True  ? AppVector { pointList = smoothVersion vector
                                                , smoothVersion = smoothVersion vector
                                                , selectedPoint = 0 } 
                                    :? vector
        (x, y) = (pointList smoothVec) !! index
        flippedPoints = map (\(x, y) -> (x, -y)) (pointList smoothVec)
        vectorLine = color black $ line
                        flippedPoints

-- Draw GUI elements in a list of elements
drawElements :: AppState -> Picture
drawElements state =
    --trace ("Drawing Elements:\n")
    pictures (map (drawElement state) (map (elemCore) (elements state)))

-- Draw everything in the window
render :: AppState -> IO Picture
render state =
    return $! applyViewPortToPicture
        (viewPortInit   { viewPortTranslate = (-winWidth / 2, winHeight / 2) })
        (pictures   [ Blank
                    , drawElements state 
                    , pictures vectorPictures
                    , currentVecPic ])
    where
        winWidth = fromIntegral (width $ window state)
        winHeight = fromIntegral (height $ window state)
        vectorPictures = map (drawVector 0 True) (drawings state)

        -- Draw little dots at each point
        -- (not . null) x === \x -> not (null x)
        selDraw =   if (selectedDrawing state) >= (length (drawings state)) then
                        AppVector { pointList = [(0, 0), (0, 0)], smoothVersion = [(0, 0), (0, 0)], selectedPoint = 10 }
                    else
                        (drawings state) !! (selectedDrawing state)
        dotsAllRed = (selectedDrawing state) >= (length (drawings state)) 
                        ? Blank
                        :? (pictures $ map (\(x, y) -> translate x (-y) (color red (circleSolid 4))) (pointList selDraw))
        (selX, selY) =  (selectedPoint selDraw) >= (length $ pointList selDraw)
                        ? (0, 0)
                        :? ((pointList selDraw) !! (selectedPoint selDraw))
        dots = selX == 0 ? Blank :? (pictures [ dotsAllRed, translate selX (-selY) (color green $ circleSolid 8) ])
        currentVecPic   | drawTool state == newDrawing && (selectedDrawing state) < (length $ drawings state) =
                            drawVector 0 True $ (drawings state) !! (selectedDrawing state)
                        | drawTool state == isMakingNewDrawing = (drawVector 0 Prelude.False (currentDrawing state))
                        | (drawTool state) == moveDrawing || (drawTool state) == isMovingDrawing && (selectedDrawing state) < (length (drawings state)) =
                            pictures [ drawVector 0 True ((drawings state) !! (selectedDrawing state)), dotsAllRed ]
                        | (drawTool state) == editDrawing || (drawTool state) == isEditingDrawing && (selectedDrawing state) < (length (drawings state)) =
                            pictures [ drawVector 0 True ((drawings state) !! (selectedDrawing state)), dots ]
                        | otherwise = Blank


updateAll :: Int -> Float -> AppState -> AppState
updateAll index seconds state =
    index == length (elements state) ? state :? updateAll (index + 1) seconds newState
    where
        -- First, update the state and get a new updated element
        element = (elements state) !! index
        elemUpdateFunc = updateElem element

        newState = elemUpdateFunc seconds state element index

-- Update everything based on new state
update :: Float -> AppState -> (IO AppState)
update seconds state =
    return fixedState
    where
        updatedState = updateAll 0 seconds state
        drawingsWOutEmptied = filter (\drawing -> (length $ pointList drawing) > 1) (drawings state)
        cleanedUpState = updatedState { drawings = drawingsWOutEmptied }
        fixedState = cleanedUpState { selectedDrawing = if (selectedDrawing cleanedUpState) >= length (drawings cleanedUpState) 
                                                            || (selectedDrawing cleanedUpState) < 0 then
                                                                0
                                                        else
                                                            selectedDrawing cleanedUpState }

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
    do
        hSetBuffering stdout NoBuffering

        drawIconPic <-          pngToPicture "images/iconset.png" (0, 0)      (512, 512) (32, 32)
        drawIconSelectedPic <-  pngToPicture "images/iconset.png" (512, 0)    (512, 512) (32, 32)
        moveIconPic <-          pngToPicture "images/iconset.png" (0, 512)    (512, 512) (32, 32)
        moveIconSelectedPic <-  pngToPicture "images/iconset.png" (512, 512)  (512, 512) (32, 32)
        editIconPic <-          pngToPicture "images/iconset.png" (0, 1024)   (512, 512) (32, 32)
        editIconSelectedPic <-  pngToPicture "images/iconset.png" (512, 1024) (512, 512) (32, 32)

        let initialState =  startState
                                drawIconPic drawIconSelectedPic
                                moveIconPic moveIconSelectedPic
                                editIconPic editIconSelectedPic
        let disp =          display $ window initialState
        let bg =            bgColor $ window initialState
        let numFrames =     fps     $ window initialState

        playIO disp bg numFrames initialState render handler update
