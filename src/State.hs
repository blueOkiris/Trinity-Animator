module State where

import Graphics.Gloss(Color, Display)
import GUI(Element(..))

-- Window type that stores info about the actual running of the app
data AppWindow = 
    AppWindow   { bgColor   :: Color
                , fps       :: Int
                , display   :: Display
                , width     :: Int
                , height    :: Int }

-- The global state type that holds all of our application's data
data AppState =
    AppState    { window    :: AppWindow 
                , elements  :: [Element] }
