module Main where

import Graphics.Gloss   ( Display(..)
                        , Picture(..), pictures, translate, circleSolid, rectangleSolid
                        , color, white, black, red, blue, green, yellow, magenta )
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game(play, Event(..), SpecialKey(..), KeyState(..), Key(..))
import Debug.Trace

import State(AppState(..), AppWindow(..))
import GUI(Element(..), Alignment, alignCenter, alignLeft, alignRight, alignTop, alignBottom, alignStretch)

startState :: AppState
startState =
    AppState    { window =  AppWindow   { bgColor = white
                                        , fps =     60
                                        , display = InWindow "Trinity Animator" (winWidth, winHeight) (200, 200)
                                        , width =   winWidth
                                        , height =  winHeight }
                , elements =    [ windowContainer
                                , Element   { borderWidth =     5
                                            , borderColor =     white
                                            , backColor =       green
                                            , backImage =       Blank
                                            , horAlignment =    alignLeft
                                            , vertAlignment =   alignStretch
                                            , offset =          ((32, 96), (256, 32))
                                            , parent =          windowContainer }
                                , Element   { borderWidth =     5
                                            , borderColor =     red
                                            , backColor =       blue
                                            , backImage =       Blank
                                            , horAlignment =    alignRight
                                            , vertAlignment =   alignStretch
                                            , offset =          ((32, 96), (256, 32))
                                            , parent =          windowContainer }
                                , Element   { borderWidth =     5
                                            , borderColor =     yellow
                                            , backColor =       magenta
                                            , backImage =       Blank
                                            , horAlignment =    alignStretch
                                            , vertAlignment =   alignStretch
                                            , offset =          ((32 + 256 + 32, 96), (32 + 256 + 32, 32 + 128 + 32))
                                            , parent =          windowContainer }
                                , Element   { borderWidth =     5
                                            , borderColor =     green
                                            , backColor =       white
                                            , backImage =       Blank
                                            , horAlignment =    alignStretch
                                            , vertAlignment =   alignBottom
                                            , offset =          ((32 + 256 + 32, 32), (32 + 256 + 32, 128))
                                            , parent =          windowContainer }
                                , Element   { borderWidth =     5
                                            , borderColor =     blue
                                            , backColor =       yellow
                                            , backImage =       Blank
                                            , horAlignment =    alignStretch
                                            , vertAlignment =   alignTop
                                            , offset =          ((32, 32), (32, 32))
                                            , parent =          windowContainer } ] }
    where
        winWidth = 1280
        winHeight = 720

        windowContainer =   Element { borderWidth =     0
                                    , borderColor =     red
                                    , backColor =       black
                                    , backImage =       Blank
                                    , horAlignment =    alignStretch
                                    , vertAlignment =   alignStretch
                                    , offset =          ((0, 0), (0, 0))
                                    , parent =          GUI.False }

-- Get parent x2
getX1, getX2, getY1, getY2 :: Element -> AppState -> Int
getX2 elem state =
    if (parent elem) == GUI.False then
        -- Base case, window, (0, 0) i.e. windowWidth
        width $ window state
    else
        if (horAlignment elem) == alignCenter then
            ((parX1 + parX2) `div` 2) + mx
        else if (horAlignment elem) == alignLeft then
            parX1 + mx + mw
        else if (horAlignment elem) == alignRight then
            parX2 - mx
        else --Stretch
            parX2 - mw
    where
        ((mx, my), (mw, mh)) = offset elem
        parX1 = getX1 (parent elem) state
        parX2 = getX2 (parent elem) state

-- Get parent x2
getY2 elem state =
    if (parent elem) == GUI.False then
        -- Base case, window, (0, 0) i.e. windowWidth
        height $ window state
    else
        if (vertAlignment elem) == alignCenter then
            ((parY1 + parY2) `div` 2) + my
        else if (vertAlignment elem) == alignTop then
            parY1 + my + mh
        else if (vertAlignment elem) == alignBottom then
            parY2 - my
        else --Stretch
            parY2 - mh
    where
        ((mx, my), (mw, mh)) = offset elem
        parY1 = getY1 (parent elem) state
        parY2 = getY2 (parent elem) state

-- Get parent x
getX1 elem state =
    if (parent elem) == GUI.False then
        -- Base case, window, (0, 0) i.e. -windowWidth / 2
        0
    else -- Calculate the x position based on parent x
        if (horAlignment elem) == alignCenter then
            ((parX1 + parX2) `div` 2) - mx
        else if (horAlignment elem) == alignLeft then
            parX1 + mx
        else if (horAlignment elem) == alignRight then
            parX2 - mw - mx
        else -- Stretch
            parX1 + mw
    where
        ((mx, my), (mw, mh)) = offset elem
        parX1 = getX1 (parent elem) state
        parX2 = getX2 (parent elem) state

-- Get parent y
getY1 elem state =
    if (parent elem) == GUI.False then
        -- Base case, window, (0, 0) i.e. -windowHeight / 2
        0
    else -- Calculate the y position based on parent y
        if (vertAlignment elem) == alignCenter then
            ((parY1 + parY2) `div` 2) - my
        else if (vertAlignment elem) == alignTop then
            parY1 + my
        else if (vertAlignment elem) == alignBottom then
            parY2 - mh - my
        else -- Stretch
            parY1 + my
    where
        ((mx, my), (mw, mh)) = offset elem
        parY1 = getY1 (parent elem) state
        parY2 = getY2 (parent elem) state
        --parY1 = trace ("Parent Y1: " ++ (show (getY1 (parent elem) state))) (getY1 (parent elem) state)
        --parY2 = trace ("Parent Y2: " ++ (show (getY2 (parent elem) state))) (getY2 (parent elem) state)

-- Draw a single element
drawElement :: AppState -> Element -> Picture
drawElement state element =
    trace ("Control:\n\t((x=" ++ (show x) ++ ", y=" ++ (show y) ++ "), (w=" ++ (show w) ++ ", h=" ++ (show h)
            ++ "))\n\t((lx=" ++ (show lx) ++ ", ly=" ++ (show ly) ++ "), (rx=" ++ (show rx) ++ ", ry=" ++ (show ry) ++ "))")
    pictures    [ translate x y (color bdCol (rectangleSolid borderW borderH))  -- Border around outside
                , translate x y (color bgCol (rectangleSolid w h))              -- Background
                , translate x y pic ]                                           -- Background image
    where
        thicknessi = borderWidth element

        bdCol = borderColor element
        bgCol = backColor element
        thickness = fromIntegral thicknessi

        lx = fromIntegral $ getX1 element state
        ly = fromIntegral $ getY1 element state
        rx = fromIntegral $ getX2 element state
        ry = fromIntegral $ getY2 element state

        x = (lx + rx) / 2
        y = -((ly + ry) / 2)
        w = rx - lx
        h = ry - ly
        borderW = w + (2 * thickness)
        borderH = h + (2 * thickness)

        pic = backImage element

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
