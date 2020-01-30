module State where

import Graphics.Gloss(Color, Display, Picture)
import Debug.Trace
import Data.List(elemIndex)

import GUI(DynamicElement(..))

-- "State" of app, but not that state
-- Like current tool stuff
type DrawTool = Int
newDrawing, isMakingNewDrawing, moveDrawing, isMovingDrawing, noState, editDrawing, isEditingDrawing :: DrawTool
newDrawing =            0
isMakingNewDrawing =    1
moveDrawing =           2
isMovingDrawing =       3
noState =               4
editDrawing =           5
isEditingDrawing =      6

-- Window type that stores info about the actual running of the app
data AppWindow = 
    AppWindow   { bgColor           :: Color
                , fps               :: Int
                , display           :: Display
                , width             :: Int
                , height            :: Int }

-- A drawing object that can be displayed on the draw
data AppVector =   
    AppVector   { pointList         :: [(Float, Float)]
                , smoothVersion     :: [(Float, Float)]
                , selectedPoint     :: Int }

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
    AppVector  { pointList =   [ (n1x, n1y), (n2x, n2y) ], smoothVersion = [ (n1x, n1y), (n2x, n2y) ], selectedPoint = 0 }
    where
        actRatio =  if ratio > 0.5 then 1 - ratio else ratio
        n1x = lerp ax bx actRatio
        n1y = lerp ay by actRatio
        n2x = lerp bx ax actRatio
        n2y = lerp by ay actRatio
chaikin iterations ratio close shape =
    -- Use Chaikin's algorithm
    -- This allows us to store fewer (and more modifiable points),
    -- but still draw with many
    if iterations == 0 then shape else chaikin (iterations - 1) ratio close next
    where
        numCorners = if not close then (length $ pointList shape) - 1 else length $ pointList shape
        newPoints = (buildNewShape ratio 0 numCorners (pointList shape) close)
        next = (AppVector   { pointList = newPoints, smoothVersion = newPoints, selectedPoint = 0 })
buildNewShape ratio i numCorners masterList close =
    if i < numCorners then
        updateList ++ (buildNewShape ratio (i + 1) numCorners masterList close)
    else
        []
    where
        a = masterList !! i
        b = masterList !! ((i + 1) `mod` (length masterList))

        n = pointList (chaikinCut ratio a b)

        appendList  | (not close) && i == 0 = [ a, n !! 1 ]
                    | (not close) && i == numCorners - 1 = [ n !! 0, b ]
                    | otherwise = [ n !! 0, n !! 1 ]
        updateList = appendList

-- The global state type that holds all of our application's data
data AppState =
    AppState    { window            :: AppWindow 
                , elements          :: [DynamicElement AppState] 
                , drawings          :: [AppVector] 
                , currentDrawing    :: AppVector
                , drawTool          :: DrawTool
                , drawIcon          :: Picture
                , drawIconSelected  :: Picture
                , moveIcon          :: Picture
                , moveIconSelected  :: Picture
                , editIcon          :: Picture
                , editIconSelected  :: Picture
                , selectedDrawing   :: Int
                , clickedDownPoint  :: (Float, Float) }

sqr :: Float -> Float
sqr x =
    x * x

pointDistance :: (Float, Float) -> (Float, Float) -> Float
pointDistance (x1, y1) (x2, y2) =
    sqrt $ (sqr (x1 - x2)) + (sqr (y1 - y2))

vectorDistances :: (Float, Float) -> [(Float, Float)] -> [Float]
vectorDistances comparePoint vectorPoints =
    map (pointDistance comparePoint) vectorPoints

vectorClosestToPoint :: AppState -> (Float, Float) -> Int
vectorClosestToPoint state comparePoint =
    minDistVector
    where
        pointLists = map pointList (drawings state)
        vecDistances = map (vectorDistances comparePoint) pointLists
        minDistances =  map minimum
                            (filter (not . null) vecDistances)
        minDistVector = maybe 0 id $ elemIndex (minimum minDistances) minDistances

vectorPointClosestToPoint :: AppState -> (Float, Float) -> Int
vectorPointClosestToPoint state comparePoint =
    --trace (show minDistPoint)
    minDistPoint
    where
        currVec = (drawings state) !! (selectedDrawing state)
        minDistances = map (pointDistance comparePoint) (pointList currVec)
        minDistPoint = maybe 0 id $ elemIndex (minimum minDistances) minDistances
