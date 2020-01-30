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

getX2 elem state
    | (parent elem) == GUI.False = width $ window state
    | (horAlignment elem) == alignCenter =  ((parX1 + parX2) `div` 2) + (mw `div` 2)
    | (horAlignment elem) == alignLeft =    parX1 + mx + mw
    | (horAlignment elem) == alignRight =   parX2 - mx
    | otherwise =                           parX2 - mw                          -- Stretch
    where
        ((mx, my), (mw, mh)) = offset elem
        parX1 = getX1 (parent elem) state
        parX2 = getX2 (parent elem) state

-- Get parent x2
getY2 elem state
    | (parent elem) == GUI.False = height $ window state
    | (vertAlignment elem) == alignCenter = ((parY1 + parY2) `div` 2) + (mh `div` 2)
    | (vertAlignment elem) == alignTop =    parY1 + my + mh
    | (vertAlignment elem) == alignBottom = parY2 - my
    | otherwise =                           parY2 - mh                          -- Stretch
    where
        ((mx, my), (mw, mh)) = offset elem
        parY1 = getY1 (parent elem) state
        parY2 = getY2 (parent elem) state

-- Get parent x
getX1 elem state
    | (parent elem) == GUI.False = 0
    | (horAlignment elem) == alignCenter =  ((parX1 + parX2) `div` 2) + mx - (mw `div` 2)
    | (horAlignment elem) == alignLeft =    parX1 + mx
    | (horAlignment elem) == alignRight =   parX2 - mw - mx
    | otherwise =                           parX1 + mw                      -- Stretch
    where
        ((mx, my), (mw, mh)) = offset elem
        parX1 = getX1 (parent elem) state
        parX2 = getX2 (parent elem) state

-- Get parent y
getY1 elem state
    | (parent elem) == GUI.False = 0
    | (vertAlignment elem) == alignCenter = ((parY1 + parY2) `div` 2) + my - (mh `div` 2)
    | (vertAlignment elem) == alignTop =    parY1 + my
    | (vertAlignment elem) == alignBottom = parY2 - mh - my
    | otherwise =                           parY1 + my                      -- Stretch
    where
        ((mx, my), (mw, mh)) = offset elem
        parY1 = getY1 (parent elem) state
        parY2 = getY2 (parent elem) state
