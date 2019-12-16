module GUI where

import Graphics.Gloss(Color, black, Picture(..), text)
import Graphics.Gloss.Juicy(loadJuicyPNG)
import Graphics.Gloss.Interface.Pure.Game
import System.IO.Unsafe(unsafePerformIO)

-- Anchoring
type Alignment = Int
alignCenter, alignLeft, alignRight, alignTop, alignBottom, alignStretch :: Alignment
alignCenter =   0
alignLeft =     1
alignTop =      1
alignRight =    2
alignBottom =   2
alignStretch =  3

-- Base GUI element
data Element =
    False |
    Element { borderWidth   :: Int
            , borderColor   :: Color
            , backColor     :: Color
            , backImage     :: Picture
            , horAlignment  :: Alignment
            , vertAlignment :: Alignment
            , offset        :: ((Int, Int), (Int, Int))
            , parent        :: Element } deriving(Eq)

-- We need element to derive Eq, but we also want to be able to have self modifying functions
-- Thus, we need a second data type that stores the element and its function
data DynamicElement appState =
    DynamicElement  { elemCore          :: Element
                    , updateElem        :: Float -> (appState, Element) -> (appState, Element)
                    , keyEventElem      :: Event -> (appState, Element) -> (appState, Element) }

-- Function to load image data from a file
{-# NOINLINE pngToPicture #-}
pngToPicture :: FilePath -> Picture
pngToPicture fname =
    maybe (text "PNG ERROR") id (unsafePerformIO $ loadJuicyPNG fname)
