module State where

import Graphics.Gloss(Color, Display)
import Debug.Trace

import GUI(DynamicElement(..))

-- "State" of app, but not that state
-- Like current tool stuff
type DrawTool = Int
newDrawing, isMakingNewDrawing, moveDrawing :: DrawTool
newDrawing = 0
isMakingNewDrawing = 1
moveDrawing = 2

-- Window type that stores info about the actual running of the app
data AppWindow = 
    AppWindow   { bgColor           :: Color
                , fps               :: Int
                , display           :: Display
                , width             :: Int
                , height            :: Int }

-- A drawing object that can be displayed on the draw
data AppVector =   
    AppVector      { pointList         :: [(Float, Float)]
                , smoothVersion     :: [(Float, Float)] }

-- Smooth a vector's points
chaikin         :: Int -> Float -> Bool -> AppVector -> AppVector
--chaikinClosed   :: Int -> Float -> AppVector -> AppVector
chaikinOpen     :: Int -> Float -> AppVector -> AppVector
chaikinCut      :: Float -> (Float, Float) -> (Float, Float) -> AppVector
lerp            :: Float -> Float -> Float -> Float
buildNewShape   :: Float -> Int -> Int -> [(Float, Float)] -> Bool -> [(Float, Float)]
--chaikinClosed iterations ratio shape =
--    chaikin iterations ratio true shape
chaikinOpen iterations ratio shape =
    chaikin iterations ratio False shape
lerp a b f =
    a + f * (b - a)
chaikinCut ratio (ax, ay) (bx, by) =
    AppVector  { pointList =   [ (n1x, n1y), (n2x, n2y) ], smoothVersion = [ (n1x, n1y), (n2x, n2y) ] }
    where
        actRatio =  if ratio > 0.5 then
                        1 - ratio
                    else
                        ratio
        n1x = lerp ax bx actRatio
        n1y = lerp ay by actRatio
        n2x = lerp bx ax actRatio
        n2y = lerp by ay actRatio
chaikin iterations ratio close shape =
    -- Use Chaikin's algorithm
    -- This allows us to store fewer (and more modifiable points),
    -- but still draw with many
    if iterations == 0 then
        shape
    else
        chaikin (iterations - 1) ratio close next
    where
        numCorners =    if close == False then
                            (length $ pointList shape) - 1
                        else
                            length $ pointList shape
        newPoints = --trace ("Master list: " ++ (show $ pointList shape) ++ "\nNum corners: " ++ (show numCorners)) 
                        (buildNewShape ratio 0 numCorners (pointList shape) close)
        next = --trace ("Final Shape: " ++ (show newPoints))
                    (AppVector   { pointList = newPoints, smoothVersion = newPoints })
buildNewShape ratio i numCorners masterList close =
    if i < numCorners then
        updateList ++ (buildNewShape ratio (i + 1) numCorners masterList close)
    else
        []
    where
        a = masterList !! i
        b = masterList !! ((i + 1) `mod` (length masterList))

        n = pointList (chaikinCut ratio a b)

        appendList =    if close == False && i == 0 then
                            [ a, n !! 1 ]
                        else if close == False && i == numCorners - 1 then
                            [ n !! 0, b ]
                        else
                            [ n !! 0, n !! 1 ]
        updateList = --trace ("Cut made: " ++ (show n) ++ ", appending: " ++ (show appendList))
                        appendList

-- The global state type that holds all of our application's data
data AppState =
    AppState    { window            :: AppWindow 
                , elements          :: [DynamicElement AppState] 
                , drawings          :: [AppVector] 
                , currentDrawing    :: AppVector
                , drawTool          :: DrawTool }
