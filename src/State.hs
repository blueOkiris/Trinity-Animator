module State where

import Graphics.Gloss(Color, Display)
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
    AppWindow   { bgColor   :: Color
                , fps       :: Int
                , display   :: Display
                , width     :: Int
                , height    :: Int }

-- A drawing object that can be displayed on the draw
data Vector =   
    Vector      { pointList :: [(Float, Float)] }

-- The global state type that holds all of our application's data
data AppState =
    AppState    { window    :: AppWindow 
                , elements  :: [DynamicElement AppState] 
                , drawings  :: [Vector] 
                , drawTool  :: DrawTool }
