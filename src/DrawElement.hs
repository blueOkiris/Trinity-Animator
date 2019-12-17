module DrawElement where

import Graphics.Gloss(Display(..), Picture(..), pictures, color, translate, rectangleSolid)
import Debug.Trace

import State(AppState(..), AppWindow(..))
import GUI  ( Element(..), Alignment, alignCenter, alignLeft, alignRight, alignTop, alignBottom, alignStretch)

-- This just extracts the very large code needed to draw the elements in a relative layout
-- This was originally in Main, so I moved it to GUI, but it needs AppState which requires GUI
-- Finally, I decided it was best to simply put all of the need functions in one file

-- Get parent x2
getX1, getX2, getY1, getY2 :: Element -> AppState -> Int

-- Draw a single element
drawElement :: AppState -> Element -> Picture
drawElement state element =
    --trace ("Control:\n\t((x=" ++ (show x) ++ ", y=" ++ (show y) ++ "), (w=" ++ (show w) ++ ", h=" ++ (show h)
    --        ++ "))\n\t((lx=" ++ (show lx) ++ ", ly=" ++ (show ly) ++ "), (rx=" ++ (show rx) ++ ", ry=" ++ (show ry) ++ "))")
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

getX2 elem state =
    if (parent elem) == GUI.False then
        -- Base case, window, (0, 0) i.e. windowWidth
        width $ window state
    else
        if (horAlignment elem) == alignCenter then
            ((parX1 + parX2) `div` 2) + (mw `div` 2)
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
            ((parY1 + parY2) `div` 2) + (mh `div` 2)
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
            ((parX1 + parX2) `div` 2) + mx - (mw `div` 2)
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
            ((parY1 + parY2) `div` 2) + my - (mh `div` 2)
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
