module GUI where

import Graphics.Gloss(Color, black, Picture(..), text)
import Graphics.Gloss.Juicy(loadJuicyPNG)
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

-- Function to load image data from a file
{-# NOINLINE pngToPicture #-}
pngToPicture :: FilePath -> Picture
pngToPicture fname =
    maybe (text "PNG ERROR") id (unsafePerformIO $ loadJuicyPNG fname)
