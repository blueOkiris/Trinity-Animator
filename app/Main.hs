module Main where

import Graphics.Gloss   ( Display(..)
                        , Picture(..), pictures, translate, circleSolid, rectangleSolid
                        , color, white, black, red, blue, green, yellow, magenta )
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game(play)

import State(AppState(..), AppWindow(..))
import GUI  ( Element(..), DynamicElement(..), Alignment, alignCenter, alignLeft, alignRight, alignTop, alignBottom, alignStretch)
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

-- Update an element inside a dynamic element based on the d.e.'s update function
updateElement :: Float -> DynamicElement -> DynamicElement
updateElement seconds de =
    de  { elemCore =    updatedCore }
    where
        updateFunc =    updateElem de
        elem =          elemCore de
        updatedCore =   updateFunc seconds elem

-- Update everything based on new state
update :: Float -> AppState -> AppState
update seconds state =
    state   { elements =    updatedElements }
    where
        updatedElements = map (updateElement seconds) (elements state)

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
