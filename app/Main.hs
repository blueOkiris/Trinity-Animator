module Main where

import Graphics.Gloss   ( Display(..)
                        , Picture(..), pictures, translate, circleSolid, rectangleSolid
                        , color, white, black, red, blue, green, yellow, magenta )
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game(play)

import State(AppState(..), AppWindow(..))
import GUI(Element(..), DynamicElement(..), Alignment, alignCenter, alignLeft, alignRight, alignTop, alignBottom, alignStretch)
import GUIObjects(updateElement)
import Init(startState)
import DrawElement
import Event(handler)

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
                    , drawElements state ])
    where
        winWidth = fromIntegral (width $ window state)
        winHeight = fromIntegral (height $ window state)

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
