module Main where

import Graphics.Gloss   ( Display(..)
                        , Picture(..), pictures, translate, circleSolid, rectangleSolid
                        , color, white, black, red, blue, green, yellow, magenta )
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game(play, Event(..), SpecialKey(..), KeyState(..), Key(..))

import State(AppState(..), AppWindow(..))
import GUI  ( Element(..), Alignment, alignCenter, alignLeft, alignRight, alignTop, alignBottom, alignStretch)
import Init(startState)
import DrawElement

-- Draw GUI elements in a list of elements
drawElements :: AppState -> Picture
drawElements state =
    --trace ("Drawing Elements:\n")
    pictures (map (drawElement state) (elements state))

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

-- Handle events for things like keys and mouse clicks
handler :: Event -> AppState -> AppState
-- Key down event handler
handler (EventKey key Down _ _) state =
    state
-- Key up event handler
handler (EventKey key Up _ _) state =
    state
-- Handle resize -> Make it so (0, 0) is always the top left
handler (EventResize newSize) state =
    state   { window = adjustedWindow }
            --, elements = adjustedElements }
    where
        (newWidth, newHeight) = newSize

        adjustedWindow = (window state) { width =   newWidth
                                        , height =  newHeight }
        --splitElements = splitAt 1 (elements state)
        --newWindowElement = ((fst splitElements) !! 0)   { position =    (newWidth `div` 2, newHeight `div` 2)
        --                                                , size =        (newWidth, newHeight) }
        --adjustedElements = newWindowElement : (snd splitElements)
-- Handle anything else
handler _ state =
    state

-- Update everything based on new state
update :: Float -> AppState -> AppState
update seconds state =
    state

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
