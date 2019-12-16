module Event where

import Graphics.Gloss.Interface.Pure.Game(Event(..), SpecialKey(..), KeyState(..), Key(..), MouseButton(..))

import State(AppWindow(..), AppState(..))
import GUI(Element(..), DynamicElement(..))
import GUIObjects(applyHandler)

-- Handle events for things like keys and mouse clicks
handler :: Event -> AppState -> AppState
-- Key down event handler
handler (EventKey key Down mod pos) state =
    state { elements = handledElems }
    where
        handledElems = map (applyHandler (EventKey key Down mod pos) state) (elements state)
-- Key up event handler
handler (EventKey key Up mod pos) state =
    state { elements = handledElems }
    where
        handledElems = map (applyHandler (EventKey key Up mod pos) state) (elements state)
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

