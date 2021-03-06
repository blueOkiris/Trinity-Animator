module GUI where

import Graphics.Gloss(Color, black, Picture(..), text)
import Graphics.Gloss.Interface.Pure.Game(Event)
import Data.Either
import Graphics.Gloss.Juicy(loadJuicyPNG, fromImageRGBA8)
import Codec.Picture
import Codec.Picture.RGBA8
import Codec.Picture.Extra(crop, scaleBilinear)
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
                    , updateElem        :: Float -> appState -> (DynamicElement appState) -> Int -> appState
                    , keyEventElem      :: Event -> appState -> (DynamicElement appState) -> Int -> appState }

imageCreator :: String -> Image PixelRGBA8
imageCreator path =
    generateImage pixelRenderer 300 300
    where
        pixelRenderer x y = PixelRGBA8 (fromIntegral x) (fromIntegral y) 128 255

-- Function to load image data from a file
-- Should only be done at start to avoid problems
pngToPicture :: FilePath -> (Int, Int) -> (Int, Int) -> (Int, Int) -> IO (Picture)
pngToPicture fname (sx, sy) (sw, sh) (w, h) =
    do
        eitherBlock <- readPng fname
        let dimg    | rights [eitherBlock] == [] = ImageRGBA8 (imageCreator "doesn't matter")
                    | otherwise = (rights [eitherBlock]) !! 0

        let newImage = fromDynamicImage dimg
        let croppedImage = scaleBilinear w h (crop sx sy sw sh newImage)
        
        let newPic = fromImageRGBA8 croppedImage

        return newPic
