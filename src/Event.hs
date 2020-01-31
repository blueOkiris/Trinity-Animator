module Event where

import Graphics.Gloss.Interface.Pure.Game(Event(..), SpecialKey(..), KeyState(..), Key(..), MouseButton(..))

import State(AppWindow(..), AppState(..))
import GUI(Element(..), DynamicElement(..))
import Lib((?), Cond(..))

handleAll :: Int -> Event -> AppState -> AppState
handleAll index event state =
    index == length (elements state) ? state :? handleAll (index + 1) event newState
    where
        -- First, update the state and get a new updated element
        element = (elements state) !! index
        elemEventHandler = keyEventElem element

        newState = elemEventHandler event state element index

-- Handle events for things like keys and mouse clicks
handler :: Event -> AppState -> (IO AppState)
-- Key down event handler
handler (EventKey key Down mod (x, y)) state =
    return newState
    where
        newState = handleAll 0 (EventKey key Down mod (x + winWidth / 2, -(y - winHeight / 2))) state
        
        winWidth = fromIntegral (width $ window state)
        winHeight = fromIntegral (height $ window state)
-- Key up event handler
handler (EventKey key Up mod (x, y)) state =
    return newState
    where
        newState = handleAll 0 (EventKey key Up mod (x + winWidth / 2, -(y - winHeight / 2))) state
        
        winWidth = fromIntegral (width $ window state)
        winHeight = fromIntegral (height $ window state)
-- Mouse movement event handler
handler (EventMotion (x, y)) state =
    return newState
    where
        newState = handleAll 0 (EventMotion (x + winWidth / 2, -(y - winHeight / 2))) state
        
        winWidth = fromIntegral (width $ window state)
        winHeight = fromIntegral (height $ window state)
-- Handle resize -> Make it so (0, 0) is always the top left
handler (EventResize newSize) state =
    return state   { window = adjustedWindow }
            --, elements = adjustedElements }
    where
        (newWidth, newHeight) = newSize

        adjustedWindow = (window state) { width =   newWidth
                                        , height =  newHeight }
                                        
-- ALL EVENTS COVERED - CODE BELOW NO LONGER NEEDED:
-- Handle anything else
--handler _ state =
--    state

